"""
dl_degrade_labels.py  (Phase 1.3)

Build the `flddeg` label: the quantity-vs-correctness control. It takes the
field label and randomly remaps field WETLAND pixels -> UPL until the global
wetland prevalence drops to the NWI source's prevalence -- so flddeg has NWI's
*amount* of wetland but field's *correctness* on the wetland pixels it keeps. If
flddeg (NWI quantity, right pixels) beats nwi (NWI quantity, wrong pixels), the
NWI gap is about correctness, not quantity.

The remap is GLOBAL and seeded: every field wetland pixel across all patches is
pooled, and exactly the number needed to hit the target prevalence is flipped,
chosen by a single seeded RNG. UPL and ignore (NaN) pixels are never touched;
flipped pixels become UPL (correctness of the survivors is preserved). The result
is written as a MOD_CLASS_FLDDEG band beside MOD_CLASS / MOD_CLASS_NWI in each
merged patch, and the seed + achieved prevalence are recorded in a manifest.

Target prevalence is MEASURED from the MOD_CLASS_NWI band in the same patches --
not hardcoded -- so it tracks whatever NWI labels are present.

Usage:
    python dl_degrade_labels.py --dry-run        # report target + flip count
    python dl_degrade_labels.py --seed 0         # write MOD_CLASS_FLDDEG
"""

import argparse
import json
import shutil
import sys
from pathlib import Path

import numpy as np
import rasterio
from tqdm import tqdm

from dl_band_utils import load_band_config

FIELD_BAND = "MOD_CLASS"
NWI_BAND = "MOD_CLASS_NWI"
FLDDEG_BAND = "MOD_CLASS_FLDDEG"
_SHOW_BARS = sys.stderr.isatty()


def class_indices(band_config: dict):
    """(wetland class indices, UPL index) from the configured class names."""
    names = band_config["class_names"]
    upl = names.index("UPL")
    wet = [i for i in range(len(names)) if i != upl]
    return wet, upl


def read_label(path: Path, band: str) -> np.ndarray:
    with rasterio.open(path) as s:
        idx = list(s.descriptions).index(band) + 1
        return s.read(idx)


def main():
    root = Path(__file__).resolve().parents[2]
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--patches-dir", type=Path,
                   default=root / "Data/Training_Data/R_Patches_Merged")
    p.add_argument("--seed", type=int, default=0, help="RNG seed for the global flip")
    p.add_argument("--config-json", type=Path, default=None)
    p.add_argument("--overwrite", action="store_true",
                   help="Rewrite MOD_CLASS_FLDDEG if it already exists")
    p.add_argument("--dry-run", action="store_true",
                   help="Compute target/flip count and write nothing")
    args = p.parse_args()

    band_config = load_band_config(args.config_json)
    wet_classes, upl = class_indices(band_config)
    wet_set = set(wet_classes)

    patches = sorted(args.patches_dir.glob("*.tif"))
    if not patches:
        print(f"No patches in {args.patches_dir}"); return 1

    # --- Pass 1: pool field wetland pixels, measure target from NWI. ---
    print(f"[1/2] Scanning {len(patches)} patches (field wetland pool + NWI target)")
    field_wet = field_labeled = nwi_wet = nwi_labeled = 0
    per_patch_wet = []  # wetland pixel count per patch, in file order
    for pf in tqdm(patches, desc="    scanning", disable=not _SHOW_BARS):
        with rasterio.open(pf) as s:
            descs = list(s.descriptions)
            if FLDDEG_BAND in descs and not args.overwrite and not args.dry_run:
                print(f"\n  [stop] {pf.name} already has {FLDDEG_BAND}; use --overwrite.")
                return 1
            fld = s.read(descs.index(FIELD_BAND) + 1)
            nwi = s.read(descs.index(NWI_BAND) + 1)
        fld_valid = ~np.isnan(fld)
        nwi_valid = ~np.isnan(nwi)
        fld_i = fld[fld_valid].astype(np.int64)
        nwi_i = nwi[nwi_valid].astype(np.int64)
        w = int(np.isin(fld_i, wet_classes).sum())
        per_patch_wet.append(w)
        field_wet += w
        field_labeled += int(fld_valid.sum())
        nwi_wet += int(np.isin(nwi_i, wet_classes).sum())
        nwi_labeled += int(nwi_valid.sum())

    field_prev = field_wet / field_labeled
    target_prev = nwi_wet / nwi_labeled
    target_wet = round(target_prev * field_labeled)
    n_flip = field_wet - target_wet

    print(f"\n  field wetland prevalence: {field_prev:.4f} ({field_wet:,}/{field_labeled:,})")
    print(f"  NWI    wetland prevalence: {target_prev:.4f} (target)")
    print(f"  flip {n_flip:,} of {field_wet:,} field wetland pixels -> UPL "
          f"(achieved prevalence {(field_wet - max(n_flip,0)) / field_labeled:.4f})")
    if n_flip <= 0:
        print("  [note] field is already at or below NWI prevalence; nothing to flip "
              "(flddeg would equal field).")
    if args.dry_run:
        print("\n--dry-run: no files written.")
        return 0

    # --- Global seeded selection of which pooled wetland pixels to flip. ---
    rng = np.random.default_rng(args.seed)
    r = rng.random(field_wet)
    selected = np.zeros(field_wet, dtype=bool)
    if n_flip > 0:
        # the n_flip pixels with the smallest random keys
        selected[np.argpartition(r, n_flip - 1)[:n_flip]] = True

    # --- Pass 2: write MOD_CLASS_FLDDEG per patch from its segment of the pool. ---
    print(f"\n[2/2] Writing {FLDDEG_BAND} (seed={args.seed})")
    offset = 0
    flipped_total = 0
    for pf, w in tqdm(list(zip(patches, per_patch_wet)), desc="    writing",
                      disable=not _SHOW_BARS):
        with rasterio.open(pf) as s:
            data = s.read()
            descs = list(s.descriptions)
            profile = s.profile.copy()
        fld = data[descs.index(FIELD_BAND)]
        flddeg = fld.copy()
        wet_mask = np.isin(np.where(np.isnan(fld), -1, fld).astype(np.int64), wet_classes)
        wet_flat = np.flatnonzero(wet_mask.ravel())          # raster-scan order
        assert len(wet_flat) == w, f"{pf.name}: wetland count drift"
        seg = selected[offset:offset + w]
        offset += w
        flip_flat = wet_flat[seg]
        if flip_flat.size:
            flat = flddeg.ravel()
            flat[flip_flat] = upl
            flddeg = flat.reshape(flddeg.shape)
            flipped_total += int(flip_flat.size)

        # Rewrite patch: existing bands + MOD_CLASS_FLDDEG (or overwrite the band).
        if FLDDEG_BAND in descs:
            out_data = data.copy()
            out_data[descs.index(FLDDEG_BAND)] = flddeg
            out_descs = descs
        else:
            out_data = np.concatenate([data, flddeg[None]], axis=0)
            out_descs = descs + [FLDDEG_BAND]
        profile.update(count=out_data.shape[0])
        tmp = pf.with_suffix(".tif.tmp")
        with rasterio.open(tmp, "w", **profile) as dst:
            dst.write(out_data)
            for i, d in enumerate(out_descs):
                dst.set_band_description(i + 1, d)
        shutil.move(str(tmp), str(pf))

    achieved = (field_wet - flipped_total) / field_labeled
    manifest = {
        "patches_dir": str(args.patches_dir), "seed": args.seed,
        "field_prevalence": round(field_prev, 6),
        "target_prevalence_nwi": round(target_prev, 6),
        "achieved_prevalence": round(achieved, 6),
        "field_wetland_px": field_wet, "flipped_px": flipped_total,
        "labeled_px": field_labeled, "added_band": FLDDEG_BAND,
        "upl_index": upl, "wetland_indices": sorted(wet_set),
    }
    (args.patches_dir / "degrade_manifest.json").write_text(json.dumps(manifest, indent=2))
    print(f"\nDone. Flipped {flipped_total:,} px -> achieved prevalence {achieved:.4f} "
          f"(target {target_prev:.4f}).")
    print(f"Manifest: {args.patches_dir / 'degrade_manifest.json'}")
    print("Next: re-run dl_make_config_stats.py --all (builds flddeg weights) and "
          "dl_preflight_check.py --require-all-labels.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
