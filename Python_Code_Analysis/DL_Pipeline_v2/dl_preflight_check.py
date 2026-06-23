"""
dl_preflight_check.py

Phase 0 preflight for the factorial label/feature experiment. Hard-fails BEFORE
any GPU time is spent if the patch set is not wired the way the experiment needs.

The label-provenance axis only means something if NWI, field, and flddeg labels
are judged on the EXACT same pixels -- same patch list, same footprints, same
predictors, differing only in the label band (Decision 4.1: multi-band labels in
each patch). This script asserts that, plus predictor parity, label-value
sanity, the 255-mask identity across label sources (Decision 4.2: NWI
non-wetland -> UPL, no-data -> 255), split determinism, and the per-config
channel counts.

It is safe to run today: configs 1-6 are field-only and data-ready, so the field
checks should pass while NWI/FLDDEG bands are reported MISSING (a warning that
gates configs 7-8, not a hard failure) until the NWI rasterization lands.

Usage:
    python dl_preflight_check.py \
        --patches-dir ../../Data/Training_Data/R_Patches \
        --stats-path ../../Data/Training_Data/multiclass_normalization_stats_wp0.5.json

    # Require every label source (use once NWI/FLDDEG bands exist):
    python dl_preflight_check.py --require-all-labels

Exit code 0 = all required checks green; 1 = a required check failed.
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np
import rasterio
from tqdm import tqdm

from dl_band_utils import (
    load_band_config,
    discover_bands_from_raster,
    compute_in_channels,
)
from dl_02_dataset import create_data_splits

# Band matrix + the 8 configs live in one place (imported by the stats subsetter
# and the Phase 2 orchestration too).
from dl_experiment_config import (
    LABEL_SOURCE_ALIASES,
    CONFIGS,
    config_bands,
    resolve_label_band,
)

VALID_LABEL_VALUES = {0, 1, 2, 3, 255}
IGNORE_VALUE = 255

# Quiet the progress bars when output is redirected to a log (keep them on a TTY).
_SHOW_BARS = sys.stderr.isatty()


class Report:
    """Collects check results; required failures drive the exit code."""

    def __init__(self):
        self.failures = 0
        self.warnings = 0

    def ok(self, name, detail=""):
        print(f"  [PASS] {name}" + (f" -- {detail}" if detail else ""))

    def fail(self, name, detail=""):
        self.failures += 1
        print(f"  [FAIL] {name}" + (f" -- {detail}" if detail else ""))

    def warn(self, name, detail=""):
        self.warnings += 1
        print(f"  [WARN] {name}" + (f" -- {detail}" if detail else ""))

    def check(self, condition, name, detail_ok="", detail_fail="", required=True):
        if condition:
            self.ok(name, detail_ok)
        elif required:
            self.fail(name, detail_fail)
        else:
            self.warn(name, detail_fail)
        return condition


def load_expected_predictors(stats_path: Path):
    """Authoritative predictor set = predictor_names from the master stats file."""
    if stats_path and stats_path.exists():
        with open(stats_path) as f:
            stats = json.load(f)
        names = stats.get("predictor_names")
        if names:
            return names, stats.get("in_channels")
    # Fall back to the full feature set defined here.
    full = BASE_BANDS + LIDAR_TIERS["chmret"] + SPECTRAL_TIERS["leafoff"]
    return full, None


def run_preflight(patches_dir: Path, stats_path: Path, config: dict,
                  seeds=(0, 1, 2), require_all_labels=False) -> int:
    rep = Report()
    patch_files = sorted(patches_dir.glob("*.tif"))

    print(f"\n=== Preflight: {patches_dir} ===")
    if not patch_files:
        rep.fail("patches present", f"no .tif files in {patches_dir}")
        return 1
    print(f"Found {len(patch_files)} patches.\n")

    expected_predictors, expected_full_channels = load_expected_predictors(stats_path)

    # --- 1. Predictor parity: every patch carries every expected predictor, same names.
    print("[1] Predictor parity (all 17 predictors present & identically named)")
    band_sets = {}
    missing_by_patch = {}
    for pf in tqdm(patch_files, desc="    scanning band names", leave=False,
                   disable=not _SHOW_BARS):
        bands = discover_bands_from_raster(pf)
        band_sets[pf] = set(bands)
        missing = [b for b in expected_predictors if b not in band_sets[pf]]
        if missing:
            missing_by_patch[pf.name] = missing
    rep.check(
        not missing_by_patch,
        "predictor parity",
        detail_ok=f"all {len(expected_predictors)} predictors in all {len(patch_files)} patches",
        detail_fail=f"{len(missing_by_patch)} patch(es) missing predictors, e.g. "
                    f"{next(iter(missing_by_patch.items()))}" if missing_by_patch else "",
    )

    # --- 2. Label-band presence per source (fld required; nwi/flddeg gate configs 7-8).
    print("\n[2] Label-source presence (multi-band labels, Decision 4.1)")
    common = set.intersection(*band_sets.values()) if band_sets else set()
    resolved = {}
    for source in ("fld", "nwi", "flddeg"):
        band = resolve_label_band(common, source)
        resolved[source] = band
        required = (source == "fld") or require_all_labels
        rep.check(
            band is not None,
            f"label source '{source}'",
            detail_ok=f"band '{band}' present in all patches",
            detail_fail=f"no band among {LABEL_SOURCE_ALIASES[source]} present "
                        f"(gates configs using label='{source}')",
            required=required,
        )

    # --- 3. Label-value sanity + per-source class prevalence (degradation target).
    print("\n[3] Label values in {0,1,2,3,255} + class prevalence")
    prevalence = {}
    masks_by_source = {}  # source -> per-patch 255 mask, for the identity check
    for source, band in resolved.items():
        if band is None:
            continue
        counts = np.zeros(256, dtype=np.int64)
        masks = {}
        bad_values = set()
        for pf in tqdm(patch_files, desc=f"    reading '{band}'", leave=False,
                       disable=not _SHOW_BARS):
            with rasterio.open(pf) as src:
                idx = list(src.descriptions).index(band) + 1
                arr = src.read(idx)
            # Treat raster nodata / NaN as the ignore value, matching the dataset loader.
            if np.issubdtype(arr.dtype, np.floating):
                arr = np.where(np.isnan(arr), IGNORE_VALUE, arr)
            arr = arr.astype(np.int64)
            vals, cnts = np.unique(arr, return_counts=True)
            for v, c in zip(vals, cnts):
                if 0 <= v <= 255:
                    counts[v] += c
                else:
                    bad_values.add(int(v))
            masks[pf.name] = (arr == IGNORE_VALUE)
        masks_by_source[source] = masks
        present_vals = {int(v) for v in np.nonzero(counts)[0]}
        stray = (present_vals - VALID_LABEL_VALUES) | bad_values
        rep.check(
            not stray,
            f"label values '{source}'",
            detail_ok=f"only {sorted(present_vals)}",
            detail_fail=f"stray values {sorted(stray)} in band '{band}'",
        )
        labeled = counts[[0, 1, 2, 3]].sum()
        if labeled:
            prevalence[source] = {
                cls: round(float(counts[i] / labeled), 4)
                for i, cls in enumerate(["EMW", "FSW", "SSW", "UPL"])
            }
    for source, prev in prevalence.items():
        wet = round(1.0 - prev["UPL"], 4)
        print(f"      {source}: wetland prevalence={wet}  {prev}")
    if "fld" in prevalence and "nwi" in prevalence:
        print(f"      -> flddeg target: degrade field wetland prevalence "
              f"{round(1.0 - prevalence['fld']['UPL'], 4)} down to NWI's "
              f"{round(1.0 - prevalence['nwi']['UPL'], 4)}")

    # --- 4. 255-mask identity between field and NWI (so the comparison is pixel-aligned).
    print("\n[4] Ignore-mask identity across label sources (Decision 4.2)")
    if masks_by_source.get("fld") and masks_by_source.get("nwi"):
        mismatches = [name for name, m in masks_by_source["fld"].items()
                      if not np.array_equal(m, masks_by_source["nwi"].get(name))]
        rep.check(
            not mismatches,
            "fld/nwi 255-mask identity",
            detail_ok="field and NWI share the same no-data mask in every patch",
            detail_fail=f"{len(mismatches)} patch(es) with differing 255 masks, "
                        f"e.g. {mismatches[:3]}",
        )
    else:
        rep.warn("fld/nwi 255-mask identity",
                 "skipped (NWI label band not present yet)")

    # --- 5. Split determinism + identical-across-configs (same seed -> same partition).
    print("\n[5] Split determinism (same seed -> identical train/val/test)")
    split_ok = True
    for seed in seeds:
        a = create_data_splits(patches_dir, seed=seed)
        b = create_data_splits(patches_dir, seed=seed)
        if [len(x) for x in a] != [len(x) for x in b] or \
           any(sorted(p.name for p in xa) != sorted(p.name for p in xb)
               for xa, xb in zip(a, b)):
            split_ok = False
        if seed == seeds[0]:
            sizes = tuple(len(x) for x in a)
    rep.check(
        split_ok,
        "split determinism",
        detail_ok=f"reproducible across seeds {list(seeds)}; "
                  f"seed {seeds[0]} -> train/val/test {sizes}",
        detail_fail="create_data_splits is not deterministic for a fixed seed",
    )
    print("      (splits key off filenames only, so they are identical across all "
          "8 configs for a given seed.)")

    # --- 6. Channel sanity for every config (one-hot expansion wired correctly).
    print("\n[6] Per-config channel counts (Geomorph one-hot 1->10)")
    band_config = load_band_config()
    for name, cfg in CONFIGS.items():
        bands = config_bands(cfg)
        n = compute_in_channels(bands, band_config)
        rep.check(
            n == cfg["channels"],
            f"channels '{name}'",
            detail_ok=f"{n} (label={cfg['label']})",
            detail_fail=f"got {n}, expected {cfg['channels']}",
        )
    if expected_full_channels is not None:
        rep.check(
            expected_full_channels == 26,
            "master stats in_channels",
            detail_ok="26 (matches fld_chmret_leafoff)",
            detail_fail=f"{expected_full_channels} in {stats_path.name}",
        )

    # --- Summary ---
    print("\n=== Summary ===")
    print(f"  failures (required): {rep.failures}")
    print(f"  warnings (gated):    {rep.warnings}")
    ready = [n for n, c in CONFIGS.items() if resolved.get(c["label"]) is not None]
    blocked = [n for n in CONFIGS if n not in ready]
    print(f"  configs runnable now: {len(ready)} -> {ready}")
    if blocked:
        print(f"  configs data-blocked: {len(blocked)} -> {blocked} "
              f"(need: {[c for c in ('nwi','flddeg') if resolved.get(c) is None]})")
    if rep.failures:
        print("\nPREFLIGHT FAILED -- fix the [FAIL] items before training.")
        return 1
    print("\nPreflight OK for the runnable configs above.")
    return 0


def main():
    project_root = Path(__file__).resolve().parents[2]
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--patches-dir", type=Path,
                        default=project_root / "Data/Training_Data/R_Patches_Merged",
                        help="Directory of GeoTIFF training patches (merged = field + MOD_CLASS_NWI)")
    parser.add_argument("--stats-path", type=Path,
                        default=project_root / "Data/Training_Data/multiclass_normalization_stats_wp0.5.json",
                        help="Master stats file (authoritative predictor_names / in_channels)")
    parser.add_argument("--config", type=Path, default=None,
                        help="Path to dl_band_config.json (default: alongside this script)")
    parser.add_argument("--seeds", type=int, nargs="+", default=[0, 1, 2],
                        help="Replication seeds whose splits are checked (default: 0 1 2)")
    parser.add_argument("--require-all-labels", action="store_true",
                        help="Treat missing NWI/FLDDEG label bands as failures, not warnings "
                             "(use once those bands exist)")
    args = parser.parse_args()

    config = load_band_config(args.config)
    rc = run_preflight(args.patches_dir, args.stats_path, config,
                       seeds=tuple(args.seeds),
                       require_all_labels=args.require_all_labels)
    sys.exit(rc)


if __name__ == "__main__":
    main()
