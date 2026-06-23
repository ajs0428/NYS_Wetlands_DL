"""
dl_merge_nwi_labels.py  (Phase 0 -> Decision 4.1)

Merge the NWI label into each field patch as a MOD_CLASS_NWI band, so every
config trains/evaluates from ONE file on ONE grid (Decision 4.1). The field
MOD_CLASS band stays as the field label (the `fld` source aliases to it); this
adds MOD_CLASS_NWI beside it. The `flddeg` band is added later by the degrade
utility (Phase 1.3) from the field band.

Pairing + the redundancy check:
  Field patch  <name>.tif   pairs with   NWI patch  NWI_<name>.tif
The name pairing is the convenience; the TRUST ANCHOR is geometry. For every
pair we hard-verify CRS + transform + width + height are identical before
copying any pixels, so a naming slip can never silently misalign labels. We also
assert the NWI label values are in {0,1,2,3,255} and that its no-data (NaN) mask
matches the field band pixel-for-pixel (Decision 4.2: NWI non-wetland is a hard
UPL label, NaN is the only ignore). ANY failed pair aborts the whole run before
writing partial output.

Non-destructive by default: writes 19-band copies to --out-dir (R_Patches_Merged),
leaving R_Patches untouched. Use --in-place to add the band to the field patches
themselves (they regenerate from R, but this overwrites them).

Usage:
    python dl_merge_nwi_labels.py --dry-run      # verify all pairs, write nothing
    python dl_merge_nwi_labels.py                # write R_Patches_Merged/
"""

import argparse
import json
import shutil
import sys
from pathlib import Path

import numpy as np
import rasterio
from tqdm import tqdm

NWI_BAND = "MOD_CLASS_NWI"
FIELD_LABEL = "MOD_CLASS"
VALID = {0, 1, 2, 3, 255}
_SHOW_BARS = sys.stderr.isatty()


def geom_key(ds):
    return (str(ds.crs), tuple(round(v, 6) for v in ds.transform[:6]), ds.width, ds.height)


def verify_pair(field_path: Path, nwi_path: Path):
    """Return (nwi_label_array, descriptions, profile) or raise ValueError on mismatch."""
    with rasterio.open(field_path) as fa, rasterio.open(nwi_path) as nb:
        if geom_key(fa) != geom_key(nb):
            raise ValueError(f"geometry mismatch:\n  {field_path.name}: {geom_key(fa)}\n"
                             f"  {nwi_path.name}: {geom_key(nb)}")
        fdesc = list(fa.descriptions)
        ndesc = list(nb.descriptions)
        if FIELD_LABEL not in fdesc:
            raise ValueError(f"{field_path.name}: no '{FIELD_LABEL}' band")
        if FIELD_LABEL not in ndesc:
            raise ValueError(f"{nwi_path.name}: no '{FIELD_LABEL}' band to read NWI label from")
        if NWI_BAND in fdesc:
            raise ValueError(f"{field_path.name}: already has a '{NWI_BAND}' band (already merged?)")

        fld = fa.read(fdesc.index(FIELD_LABEL) + 1)
        nwi = nb.read(ndesc.index(FIELD_LABEL) + 1)
        profile = fa.profile.copy()

    # Value sanity on NWI (treat NaN as ignore for the check, keep NaN in the data).
    nwi_check = np.where(np.isnan(nwi), 255, nwi).astype(np.int64)
    stray = set(np.unique(nwi_check)) - VALID
    if stray:
        raise ValueError(f"{nwi_path.name}: stray NWI label values {sorted(stray)}")

    # 255/NaN-mask identity (Decision 4.2): same ignore pixels in field and NWI.
    if not np.array_equal(np.isnan(fld), np.isnan(nwi)):
        raise ValueError(f"{field_path.name}: NWI no-data mask differs from field")

    return nwi.astype(np.float32), profile


def write_merged(field_path: Path, nwi_label: np.ndarray, out_path: Path):
    """Copy the field patch's 18 bands and append MOD_CLASS_NWI as band 19."""
    with rasterio.open(field_path) as src:
        data = src.read()                       # (18, H, W)
        descs = list(src.descriptions)
        profile = src.profile.copy()
    profile.update(count=data.shape[0] + 1)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(out_path, "w", **profile) as dst:
        for i in range(data.shape[0]):
            dst.write(data[i], i + 1)
            dst.set_band_description(i + 1, descs[i])
        dst.write(nwi_label, data.shape[0] + 1)
        dst.set_band_description(data.shape[0] + 1, NWI_BAND)


def main():
    root = Path(__file__).resolve().parents[2]
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--field-dir", type=Path, default=root / "Data/Training_Data/R_Patches")
    p.add_argument("--nwi-dir", type=Path, default=root / "Data/Training_Data/R_Patches_NWI")
    p.add_argument("--out-dir", type=Path, default=root / "Data/Training_Data/R_Patches_Merged",
                   help="Destination for 19-band merged patches (non-destructive default)")
    p.add_argument("--in-place", action="store_true",
                   help="Add MOD_CLASS_NWI to the field patches themselves (overwrites R_Patches)")
    p.add_argument("--dry-run", action="store_true",
                   help="Verify every pair (geometry/mask/values) and write nothing")
    args = p.parse_args()

    field_files = sorted(args.field_dir.glob("*.tif"))
    if not field_files:
        print(f"No patches in {args.field_dir}"); return 1
    nwi_names = {f.name for f in args.nwi_dir.glob("*.tif")}

    # --- Pass 1: pair + verify EVERY patch before writing anything. ---
    print(f"[1/2] Verifying {len(field_files)} field<->NWI pairs (geometry is the trust anchor)")
    pairs, failures, fld_v, nwi_v = [], [], np.zeros(256, np.int64), np.zeros(256, np.int64)
    for ff in tqdm(field_files, desc="    verifying", disable=not _SHOW_BARS):
        nf = args.nwi_dir / f"NWI_{ff.name}"
        if nf.name not in nwi_names:
            failures.append((ff.name, "no NWI partner")); continue
        try:
            nwi_label, _ = verify_pair(ff, nf)
        except ValueError as e:
            failures.append((ff.name, str(e))); continue
        pairs.append((ff, nf, nwi_label))
        nc = np.where(np.isnan(nwi_label), 255, nwi_label).astype(np.int64)
        for v, c in zip(*np.unique(nc, return_counts=True)):
            nwi_v[v] += c

    if failures:
        print(f"\n  [FAIL] {len(failures)} pair(s) did not verify -- nothing written:")
        for name, why in failures[:10]:
            print(f"    {name}: {why}")
        return 1
    print(f"  [PASS] all {len(pairs)} pairs verified (geometry + mask + label values)")

    if args.dry_run:
        print("\n--dry-run: verification only, no files written.")
        return 0

    # --- Pass 2: write merged patches. ---
    out_dir = args.field_dir if args.in_place else args.out_dir
    mode = "in place (overwriting R_Patches)" if args.in_place else f"to {out_dir}"
    print(f"\n[2/2] Writing {len(pairs)} merged 19-band patches {mode}")
    for ff, nf, nwi_label in tqdm(pairs, desc="    writing", disable=not _SHOW_BARS):
        out_path = ff if args.in_place else out_dir / ff.name
        tmp = out_path.with_suffix(".tif.tmp")
        write_merged(ff, nwi_label, tmp)
        shutil.move(str(tmp), str(out_path))

    manifest = {
        "field_dir": str(args.field_dir), "nwi_dir": str(args.nwi_dir),
        "out_dir": str(out_dir), "in_place": args.in_place,
        "n_patches": len(pairs), "added_band": NWI_BAND,
        "field_label_band": FIELD_LABEL,
    }
    (out_dir / "merge_manifest.json").write_text(json.dumps(manifest, indent=2))
    print(f"\nDone. {len(pairs)} patches now carry {NWI_BAND} beside {FIELD_LABEL}.")
    print(f"Manifest: {out_dir / 'merge_manifest.json'}")
    print("Next: regenerate master stats over the merged dir (predictors unchanged, "
          "band_names gains MOD_CLASS_NWI), then re-run dl_preflight_check.py --require-all-labels.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
