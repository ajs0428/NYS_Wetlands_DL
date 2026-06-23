"""
dl_make_config_stats.py  (Phase 1.1 + 1.4)

Derive a per-config normalization-stats file from the master stats file by
subsetting the active predictor bands -- no rescan of patches required.

Why this is the whole of band-subsetting: dl_02_dataset.WetlandPatchDataset
selects which raster bands to load from `predictor_names` in the stats file and
recomputes nothing about the band set itself. So a stats file whose
`predictor_names` lists only a config's active bands trains that config on
exactly that subset. `band_names` is kept as the FULL raster band list so band
indices and the dataset's band-count filter still resolve against the real
patches.

Master stats: the production file is computed over all 17 predictors (26
channels) at weight_power 0.5, so it already IS config 6 (fld_chmret_leafoff)
and the source every reduced-band config is carved from.

Class weights: for label source `fld` the master's weights apply unchanged (its
label band MOD_CLASS is the field label). For `nwi` / `flddeg` the weights must
be recomputed from that label band's pixel counts at the same weight_power --
that needs the merged NWI/FLDDEG label bands and is gated until they exist; this
tool refuses to fabricate field weights for a non-field source.

Usage:
    # One config:
    python dl_make_config_stats.py --config fld_chm_leafon
    # All field-ready configs (skips nwi/flddeg unless --allow-nonfield):
    python dl_make_config_stats.py --all
"""

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
import rasterio
from tqdm import tqdm

from dl_band_utils import (
    load_band_config, compute_in_channels, stats_filename,
    discover_bands_from_raster,
)
from dl_experiment_config import CONFIGS, config_bands, get_config, LABEL_SOURCE_ALIASES


def subset_stats(master: dict, cfg_name: str, cfg: dict, band_config: dict,
                 raster_bands: list) -> dict:
    """Return a new stats dict for `cfg` carved from the master stats dict.

    `raster_bands` is the FULL band list of the actual training patches (read
    from a real file), so band_names tracks the data even as label bands are
    added by the merge/degrade steps. The dataset filters patches on
    len(band_names) and indexes predictors/label by position into it, so this
    must match the raster exactly.
    """
    active = config_bands(cfg)

    # Master must be a superset of the active predictors (it is the full set).
    master_predictors = set(master["predictor_names"])
    missing = [b for b in active if b not in master_predictors]
    if missing:
        raise ValueError(
            f"config '{cfg_name}': master stats lacks predictor(s) {missing}. "
            f"Master predictors: {sorted(master_predictors)}"
        )

    # Every active predictor must exist in the actual raster too.
    missing_raster = [b for b in active if b not in raster_bands]
    if missing_raster:
        raise ValueError(f"config '{cfg_name}': patches lack band(s) {missing_raster}")

    # The config's label band must be present in the raster.
    label_band = next((a for a in LABEL_SOURCE_ALIASES[cfg["label"]] if a in raster_bands), None)
    if label_band is None:
        raise ValueError(
            f"config '{cfg_name}': none of {LABEL_SOURCE_ALIASES[cfg['label']]} in patches")

    in_channels = compute_in_channels(active, band_config)
    if in_channels != cfg["channels"]:
        raise AssertionError(
            f"config '{cfg_name}': subset resolves to {in_channels} channels, "
            f"plan expects {cfg['channels']}"
        )

    out = dict(master)  # shallow copy; we replace the predictor-scoped fields below
    out["config_name"] = cfg_name
    out["label_source"] = cfg["label"]
    out["label_band"] = label_band                        # active source's band
    out["band_names"] = list(raster_bands)                # full real raster order
    out["predictor_names"] = active                       # <-- the subset
    out["in_channels"] = in_channels                      # recomputed
    out["normalization"] = {b: master["normalization"][b] for b in active}
    out["band_statistics"] = {b: master["band_statistics"][b] for b in active}
    # class_* default to the master's (field) values; for nwi/flddeg they are
    # overwritten by recompute_label_stats() from that source's own label band.
    return out


def power_scaled_weights(class_counts: dict, n_classes: int, weight_power: float):
    """Normalized (1/freq)**power class weights -- mirrors dl_01_compute_statistics.

    Weights are normalized so the minimum non-zero weight is 1.0, matching the
    master file dl_01 writes, so fld and nwi/flddeg weights are on the same scale
    and only the power (held at 0.5) is fixed across the experiment.
    """
    total = sum(class_counts.values())
    freqs = {k: v / total for k, v in class_counts.items()} if total else {}
    weights = {}
    for i in range(n_classes):
        f = freqs.get(i, 0)
        weights[i] = (1.0 / f) ** weight_power if f > 0 else 0.0
    non_zero = [w for w in weights.values() if w > 0]
    if non_zero:
        m = min(non_zero)
        weights = {k: (v / m if v > 0 else 0.0) for k, v in weights.items()}
    return freqs, weights


def count_label_band(patch_files, label_band: str, n_classes: int, label_remap=None):
    """Tally class pixel counts for one label band across patches (NaN = ignore)."""
    counts = defaultdict(int)
    for pf in tqdm(patch_files, desc=f"    counting {label_band}", leave=False,
                   disable=not sys.stderr.isatty()):
        with rasterio.open(pf) as s:
            idx = list(s.descriptions).index(label_band) + 1
            arr = s.read(idx)
        valid = arr[~np.isnan(arr)].astype(np.int64)
        if label_remap:
            remapped = np.full(valid.shape, -1, dtype=np.int64)
            for src, dst in label_remap.items():
                remapped[valid == int(src)] = dst
            valid = remapped[remapped >= 0]
        for v, c in zip(*np.unique(valid, return_counts=True)):
            if 0 <= v < n_classes:
                counts[int(v)] += int(c)
    return counts


def recompute_label_stats(out: dict, patch_files, label_band: str, master: dict):
    """Overwrite out's class counts/frequencies/weights from `label_band`'s pixels.

    Needed for nwi/flddeg: their class weights must come from their OWN label
    band (field, NWI, and flddeg prevalences differ), at the same weight_power.
    """
    class_names = master["class_names"]
    n = len(class_names)
    wp = master.get("weight_power")
    remap = master.get("label_remap")
    remap = {int(k): v for k, v in remap.items()} if remap else None

    counts = count_label_band(patch_files, label_band, n, remap)
    labeled = sum(counts.values())
    freqs, weights = power_scaled_weights(counts, n, wp)
    out["labeled_pixels"] = labeled
    out["class_counts"] = {class_names[k]: counts.get(k, 0) for k in range(n)}
    out["class_frequencies"] = {class_names[k]: round(freqs.get(k, 0), 6) for k in range(n)}
    out["class_weights"] = {class_names[k]: round(weights.get(k, 0), 4) for k in range(n)}
    return out


def main():
    project_root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--master-stats", type=Path,
                        default=project_root / "Data/Training_Data/multiclass_normalization_stats_wp0.5.json",
                        help="Master stats file (full predictor set, weight_power 0.5)")
    parser.add_argument("--patches-dir", type=Path,
                        default=project_root / "Data/Training_Data/R_Patches_Merged",
                        help="Training patches the configs run on (band_names read from here)")
    parser.add_argument("--out-dir", type=Path,
                        default=project_root / "Data/Training_Data/stats",
                        help="Directory to write per-config stats files")
    parser.add_argument("--config", type=str, default=None,
                        help="Single config name to build (default: with --all, all configs)")
    parser.add_argument("--all", action="store_true",
                        help="Build every config (a config whose label band is absent is skipped)")
    parser.add_argument("--config-json", type=Path, default=None,
                        help="Path to dl_band_config.json (default: alongside this script)")
    args = parser.parse_args()

    if not args.config and not args.all:
        parser.error("specify --config <name> or --all")

    with open(args.master_stats) as f:
        master = json.load(f)
    band_config = load_band_config(args.config_json)

    # band_names tracks the ACTUAL patches (merge/degrade may have added label bands).
    patch_files = sorted(args.patches_dir.glob("*.tif"))
    if not patch_files:
        parser.error(f"no .tif patches in {args.patches_dir}")
    raster_bands = discover_bands_from_raster(patch_files[0])
    print(f"[patches] {len(patch_files)} files in {args.patches_dir.name}, "
          f"{len(raster_bands)} bands: ...{raster_bands[-3:]}")

    # Sanity: the master must be the wp0.5 multiclass full-feature file.
    if master.get("weight_power") != 0.5:
        print(f"[warn] master weight_power={master.get('weight_power')} (expected 0.5)")
    if master.get("in_channels") != 26:
        print(f"[warn] master in_channels={master.get('in_channels')} (expected 26 for full set)")

    names = [args.config] if args.config else list(CONFIGS)
    args.out_dir.mkdir(parents=True, exist_ok=True)

    built, skipped = [], []
    for name in names:
        cfg = get_config(name)
        try:
            out = subset_stats(master, name, cfg, band_config, raster_bands)
        except ValueError as e:
            # Label band not present yet (e.g. flddeg before the degrade step).
            skipped.append((name, str(e)))
            print(f"[skip] {name}: {e}")
            continue

        # fld keeps the master's (field) class weights; nwi/flddeg recompute from
        # their own label band at the same weight_power.
        if cfg["label"] != "fld":
            recompute_label_stats(out, patch_files, out["label_band"], master)

        # Mode-prefixed, config-tagged, wp-suffixed filename per the pipeline convention.
        base = stats_filename(master.get("classification_mode", "multiclass"),
                              master.get("weight_power"))
        fname = base.replace("_normalization_stats", f"_normalization_stats_{name}")
        out_path = args.out_dir / fname
        with open(out_path, "w") as f:
            json.dump(out, f, indent=2)
        built.append((name, out["in_channels"], out_path.name))
        wsrc = "field (master)" if cfg["label"] == "fld" else f"recomputed from {out['label_band']}"
        print(f"[ok]  {name:24s} in_channels={out['in_channels']:2d}  weights: {wsrc}")
        if cfg["label"] != "fld":
            print(f"         class_weights={out['class_weights']}")

    print(f"\nBuilt {len(built)} stats file(s) in {args.out_dir}")
    if skipped:
        print(f"Skipped (label band absent): {[n for n, _ in skipped]}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
