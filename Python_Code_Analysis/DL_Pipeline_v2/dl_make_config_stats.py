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

Normalization master: the MULTICLASS production file (all 17 predictors, 26
channels, weight_power 0.5) is the single normalization source for BOTH modes --
its min/max come from a global raster scan (predictor-only, so mode-invariant),
verified to cover the current patches. `--mode` only sets class metadata
(class_names/label_remap via dl_band_config) and the output filename prefix; the
separate binary master is no longer used (it drifted from the multiclass one).

Class weights (v2): label sources live in SEPARATE directories (plan 4.1), each
carrying a single `MOD_CLASS` band, so EVERY config's weights are recomputed by
counting `MOD_CLASS` across that config's own patch directory(ies)
(`config_patch_dirs`) -- never trusting the master's field weights, which went
stale when the patches were re-cut:
  * `fld*`      -> R_Patches.
  * `nwi`       -> R_Patches_NWI.
  * `nwiextra`  -> R_Patches_NWI + R_Patches_NWIextra (the ~2x pool).
  * `nwifield`  -> R_Patches + R_Patches_NWIextra (field labels + NWI extras).
  * `flddeg`    -> R_Patches with the analytic wetland->UPL degrade applied at the
                   measured flip probability (dl_degrade_labels.degraded_class_counts),
                   so its weights reflect the NWI-degraded distribution.
Counting folds multiclass->mode, so binary weights come from the same pixels.
Weights are counted over the whole source (seed-independent, so one stats file
serves all seeds); this aggregate prevalence is a deliberate, negligible
approximation of the per-seed train split's and uses no test *learning*.

Usage:
    python dl_make_config_stats.py --config nwiextra_chmret_leafoff --mode multiclass
    python dl_make_config_stats.py --all --mode multiclass   # all but flddeg (gated)
    python dl_make_config_stats.py --all --mode binary        # same, binary master
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
from dl_experiment_config import (
    CONFIGS, config_bands, get_config, LABEL_SOURCE_ALIASES,
    config_patch_dirs, FIELD_TEST_DIR,
)


def mode_class_meta(mode: str, band_config: dict):
    """(class_names, label_remap) for a classification mode, from dl_band_config.

    Mode-definitional, not data-dependent: multiclass uses the configured class
    names with no remap; binary folds via `binary_mapping` -> [WET, UPL] with a
    multiclass-index -> binary-index remap. This lets per-config stats carry the
    right class metadata for either mode while sourcing NORMALIZATION from the
    single (multiclass) master, whose global stats are predictor-only and so mode-
    invariant.
    """
    if mode == "multiclass":
        return list(band_config["class_names"]), None
    bmap = band_config["binary_mapping"]                 # {WET:[EMW,FSW,SSW], UPL:[UPL]}
    class_names = list(bmap.keys())                      # [WET, UPL]
    mc_names = band_config["class_names"]                # [EMW, FSW, SSW, UPL]
    name_to_bin = {mc: bi for bi, bname in enumerate(class_names) for mc in bmap[bname]}
    remap = {mc_i: name_to_bin[mc] for mc_i, mc in enumerate(mc_names)}
    return class_names, remap


def subset_stats(master: dict, cfg_name: str, cfg: dict, band_config: dict,
                 raster_bands: list, mode: str = "multiclass") -> dict:
    """Return a new stats dict for `cfg` carved from the (multiclass) master.

    Normalization/band_statistics come from the master (predictor-only global
    stats, valid for both modes); class metadata (class_names/label_remap/
    classification_mode) is set for `mode`. `raster_bands` is the FULL band list of
    the actual training patches, so band_names tracks the data; the dataset filters
    patches on len(band_names) and indexes predictors/label by position into it.
    Class counts/frequencies/weights are filled in by recompute_label_stats().
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

    class_names, label_remap = mode_class_meta(mode, band_config)

    out = dict(master)  # shallow copy; we replace the predictor-/mode-scoped fields below
    out["config_name"] = cfg_name
    out["label_source"] = cfg["label"]
    out["label_band"] = label_band                        # active source's band (MOD_CLASS)
    out["band_names"] = list(raster_bands)                # full real raster order
    out["predictor_names"] = active                       # <-- the subset
    out["in_channels"] = in_channels                      # recomputed
    out["normalization"] = {b: master["normalization"][b] for b in active}
    out["band_statistics"] = {b: master["band_statistics"][b] for b in active}
    out["classification_mode"] = mode
    out["class_names"] = class_names
    out["label_remap"] = {str(k): v for k, v in label_remap.items()} if label_remap else None
    # class_counts/frequencies/weights are always recomputed from disk (below).
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


def fold_counts_to_mode(mc_counts: dict, mode: str, band_config: dict):
    """Fold 4-class multiclass counts to the mode's class-index counts + names."""
    class_names, remap = mode_class_meta(mode, band_config)
    if remap is None:
        return {int(k): int(v) for k, v in mc_counts.items()}, class_names
    folded = defaultdict(int)
    for mc_i, c in mc_counts.items():
        folded[remap[int(mc_i)]] += int(c)
    return dict(folded), class_names


def recompute_label_stats(out: dict, patch_files, mode: str, band_config: dict,
                          weight_power: float, degrade_p: float = None):
    """Fill class counts/frequencies/weights from disk (v2, all configs).

    Counts MOD_CLASS in multiclass space over the config's own patch dir(s); for
    flddeg applies the analytic wetland->UPL degrade at `degrade_p`; folds to the
    mode's classes; then power-scales weights. Replaces trusting the master's
    (now stale) field weights -- every config's weights come from its real pixels.
    """
    from dl_degrade_labels import degraded_class_counts

    mc_counts = count_label_band(patch_files, "MOD_CLASS", 4, None)
    if degrade_p:
        mc_counts = degraded_class_counts(mc_counts, degrade_p, (0, 1, 2), 3)
    counts, class_names = fold_counts_to_mode(mc_counts, mode, band_config)
    n = len(class_names)
    labeled = sum(counts.values())
    freqs, weights = power_scaled_weights(counts, n, weight_power)
    out["labeled_pixels"] = labeled
    out["class_counts"] = {class_names[k]: int(counts.get(k, 0)) for k in range(n)}
    out["class_frequencies"] = {class_names[k]: round(freqs.get(k, 0), 6) for k in range(n)}
    out["class_weights"] = {class_names[k]: round(weights.get(k, 0), 4) for k in range(n)}
    return out


def config_source_files(data_root: Path, name: str) -> list:
    """All patch files supplying a config's TRAIN/VAL labels, across its dir(s).

    v2 counts MOD_CLASS over the config's own label directory(ies) rather than a
    label band in one merged file. Raises if any directory is missing or empty so
    a mis-staged source fails loudly instead of yielding zero-count weights.
    """
    files = []
    for d in config_patch_dirs(name):
        dpath = data_root / d
        if not dpath.is_dir():
            raise FileNotFoundError(f"config '{name}': patch dir not found: {dpath}")
        found = sorted(dpath.glob("*.tif"))
        if not found:
            raise ValueError(f"config '{name}': no .tif patches in {dpath}")
        files += found
    return files


def main():
    project_root = Path(__file__).resolve().parents[2]
    data_default = project_root / "Data/Training_Data"
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--mode", choices=["multiclass", "binary"], default="multiclass",
                        help="Classification mode: sets class metadata + output filename prefix "
                             "(normalization always comes from the multiclass master)")
    parser.add_argument("--master-stats", type=Path, default=None,
                        help="Normalization master (default: <data-root>/multiclass_normalization_stats_wp0.5.json)")
    parser.add_argument("--data-root", type=Path, default=data_default,
                        help="Dir holding the v2 patch directories (R_Patches, R_Patches_NWI, ...)")
    parser.add_argument("--band-ref-dir", type=str, default=FIELD_TEST_DIR,
                        help="Patch dir (under --data-root) whose bands define band_names (all v2 dirs share schema)")
    parser.add_argument("--out-dir", type=Path, default=None,
                        help="Directory to write per-config stats files (default: <data-root>/stats)")
    parser.add_argument("--config", type=str, default=None,
                        help="Single config name to build (default: with --all, all configs)")
    parser.add_argument("--all", action="store_true",
                        help="Build every config (flddeg is skipped -- gated on the degrade step)")
    parser.add_argument("--config-json", type=Path, default=None,
                        help="Path to dl_band_config.json (default: alongside this script)")
    args = parser.parse_args()

    if not args.config and not args.all:
        parser.error("specify --config <name> or --all")

    # Normalization always comes from the MULTICLASS master (predictor-only global
    # stats, mode-invariant, verified to cover the current patches). --mode only
    # sets class metadata + output prefix; the stale/inconsistent binary master is
    # no longer used.
    master_path = args.master_stats or (args.data_root / "multiclass_normalization_stats_wp0.5.json")
    out_dir = args.out_dir or (args.data_root / "stats")
    with open(master_path) as f:
        master = json.load(f)
    band_config = load_band_config(args.config_json)
    wp = master.get("weight_power", 0.5)

    # band_names tracks the ACTUAL v2 patches; all label dirs share the 18-band
    # schema, so the field dir is the reference.
    band_ref = args.data_root / args.band_ref_dir
    ref_files = sorted(band_ref.glob("*.tif"))
    if not ref_files:
        parser.error(f"no .tif patches in band reference dir {band_ref}")
    raster_bands = discover_bands_from_raster(ref_files[0])
    print(f"[mode={args.mode}] norm-master={master_path.name} | band ref {band_ref.name} "
          f"({len(raster_bands)} bands: ...{raster_bands[-3:]})")

    # Sanity: the master must be the wp0.5 multiclass full-feature file.
    if wp != 0.5:
        print(f"[warn] master weight_power={wp} (expected 0.5)")
    if master.get("in_channels") != 26:
        print(f"[warn] master in_channels={master.get('in_channels')} (expected 26 for full set)")
    if master.get("classification_mode") != "multiclass":
        print(f"[warn] norm-master classification_mode={master.get('classification_mode')} "
              f"(expected multiclass -- it is the normalization source for both modes)")

    names = [args.config] if args.config else list(CONFIGS)
    out_dir.mkdir(parents=True, exist_ok=True)

    # flddeg degrade probability, measured once from field vs NWI wetland prevalence.
    flddeg_p = None
    if any(get_config(n)["label"] == "flddeg" for n in names):
        from dl_degrade_labels import flip_prob
        fld_mc = count_label_band(config_source_files(args.data_root, "fld_chmret_leafoff"),
                                  "MOD_CLASS", 4, None)
        nwi_mc = count_label_band(sorted((args.data_root / "R_Patches_NWI").glob("*.tif")),
                                  "MOD_CLASS", 4, None)
        fw = sum(fld_mc[i] for i in (0, 1, 2)) / sum(fld_mc.values())
        nw = sum(nwi_mc[i] for i in (0, 1, 2)) / sum(nwi_mc.values())
        flddeg_p = flip_prob(fw, nw)
        print(f"[flddeg] field_wet={fw:.4f} nwi_wet={nw:.4f} -> flip_prob={flddeg_p:.4f}")

    built, skipped = [], []
    for name in names:
        cfg = get_config(name)
        try:
            out = subset_stats(master, name, cfg, band_config, raster_bands, mode=args.mode)
            # Every config's weights come from ITS OWN patch dir(s) on disk (never
            # the master's now-stale field weights); flddeg additionally degrades.
            src_files = config_source_files(args.data_root, name)
            dp = flddeg_p if cfg["label"] == "flddeg" else None
            recompute_label_stats(out, src_files, args.mode, band_config, wp, degrade_p=dp)
        except (ValueError, FileNotFoundError) as e:
            skipped.append((name, str(e)))
            print(f"[skip] {name}: {e}")
            continue

        # Mode-prefixed, config-tagged, wp-suffixed filename per the pipeline convention.
        base = stats_filename(args.mode, wp)
        fname = base.replace("_normalization_stats", f"_normalization_stats_{name}")
        out_path = out_dir / fname
        with open(out_path, "w") as f:
            json.dump(out, f, indent=2)
        built.append((name, out["in_channels"], out_path.name))
        src = " + ".join(config_patch_dirs(name))
        if cfg["label"] == "flddeg":
            src += f" +degrade(p={flddeg_p:.3f})"
        print(f"[ok]  {name:24s} in_channels={out['in_channels']:2d}  weights from {src}")
        print(f"         class_weights={out['class_weights']}")

    print(f"\nBuilt {len(built)} {args.mode} stats file(s) in {out_dir}")
    if skipped:
        print(f"Skipped: {[n for n, _ in skipped]}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
