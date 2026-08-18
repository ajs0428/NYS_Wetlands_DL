"""
dl_preflight_check.py  (factorial v3, plan Phase 0)

Hard gate run on the CPU node BEFORE any GPU time: fails if the patch set is
not wired the way the experiment needs. Each label source lives in its OWN
directory (Decision 4.1) -- R_Patches (field), R_Patches_NWI (paired), and
R_Patches_NWIextra (new locations) -- each with a single MOD_CLASS band, so the
checks are built around directory-aware keys and the field-anchored pools:

  [0] Directory presence + counts (every dir any config needs).
  [1] Off-size patches (WARN): flags non-256x256 patches -- the dataset silently
      skips them (modal-size filter), so they should be known, not surprising.
  [2] Predictor parity: every predictor + MOD_CLASS present & identically named
      in every patch of every dir (authoritative set = stats predictor_names).
      This is also the cross-directory band-set guard: v3 added three terrain
      metrics (21-band patches), and a dir left at the old schema would other-
      wise be dropped SILENTLY by the dataset's band-count filter rather than
      raising -- see dl_02_dataset.py's "Skipping ...: N bands" path.
  [3] Field<->NWI pairing (every field patch twinned; reverse orphans WARN only)
      + NWIextra HUC12s subset field.
  [4] Footprint identity per paired twin: field & NWI share CRS/transform/H/W/
      nodata (pixel-aligned) -- the paired nwi-vs-fld contrast depends on it.
  [5] Label values in {0,1,2,3,255} per dir + wetland prevalence (flddeg target).
  [6] NWI 255-mask == field 255-mask per twin (Decision 4.2 alignment).
  [7] Field-anchored split + leakage gate: for each seed, resolve_pools succeeds
      (it self-asserts no test footprint reaches train/val) and the test set is
      identical across all 8 configs and drawn only from field.
  [8] Per-config channel counts + per-config/per-mode stats files present.
  [9] Fusion branch partition (--arch mbfusion): branch slices are disjoint,
      cover every channel exactly once, and keep the Geomorph one-hot block
      contiguous inside the terrain branch. Derived from the STATS file's
      predictor_names (what the dataset indexes by), so it also catches drift
      between the stats files and the config registry.

Pixel-level checks ([4]-[6]) accept --sample N to bound I/O on slow storage
(default: all). Exit 0 = all required checks green; 1 = a required check failed.

Usage:
    python dl_preflight_check.py                         # default data-root/stats-dir
    python dl_preflight_check.py --sample 120            # quick pixel checks
    python dl_preflight_check.py --leakage-guard coord   # validate the sensitivity split
"""

import argparse
import sys
from pathlib import Path

import numpy as np
import rasterio
from tqdm import tqdm

from dl_band_utils import load_band_config, discover_bands_from_raster, compute_in_channels
from dl_experiment_config import (
    CONFIGS, config_bands, config_patch_dirs, config_pool_rule,
    FIELD_TEST_DIR, LEAKAGE_GUARD,
    field_key, nwi_field_twin, huc12_of, stats_basename,
    verify_branch_partition,
)
import dl_patch_pools as P

VALID_LABEL_VALUES = {0, 1, 2, 3, 255}
IGNORE_VALUE = 255
PATCH_SIZE = 256
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
        return bool(condition)


def _header(path: Path) -> dict:
    """Cheap per-patch header: band names, dims, grid, nodata (no pixel read)."""
    with rasterio.open(path) as s:
        return {
            "bands": tuple(s.descriptions),
            "hw": (s.height, s.width),
            "transform": tuple(s.transform)[:6],
            "epsg": s.crs.to_epsg() if s.crs else None,
            "nodata": s.nodata,
        }


def _grid_equal(a: dict, b: dict) -> bool:
    """Two patch headers describe the same grid pixel-for-pixel.

    NaN-aware on nodata (nan != nan under ==) and tolerant on the affine transform
    (float round-trip), so identical grids are not falsely flagged.
    """
    if a["hw"] != b["hw"] or a["epsg"] != b["epsg"]:
        return False
    if not np.allclose(a["transform"], b["transform"], rtol=0, atol=1e-6):
        return False
    na, nb = a["nodata"], b["nodata"]
    na_nan = isinstance(na, float) and np.isnan(na)
    nb_nan = isinstance(nb, float) and np.isnan(nb)
    if na_nan or nb_nan:
        return na_nan and nb_nan
    return na == nb


def _read_label(path: Path, band: str = "MOD_CLASS") -> np.ndarray:
    """MOD_CLASS as int64 with NaN/nodata -> IGNORE (matches the dataset loader)."""
    with rasterio.open(path) as s:
        idx = list(s.descriptions).index(band) + 1
        arr = s.read(idx)
        nod = s.nodata
    if np.issubdtype(arr.dtype, np.floating):
        arr = np.where(np.isnan(arr), IGNORE_VALUE, arr)
    arr = arr.astype(np.int64)
    if nod is not None and not (isinstance(nod, float) and np.isnan(nod)):
        arr = np.where(arr == int(nod), IGNORE_VALUE, arr)
    return arr


def _sampled(files, sample):
    """Deterministic evenly-spaced subsample of a file list (or all if sample None)."""
    if sample is None or sample >= len(files):
        return files
    step = max(1, len(files) // sample)
    return files[::step][:sample]


def run_preflight(data_root: Path, stats_dir: Path, norm_master: Path,
                  seeds=(0, 1, 2), leakage_guard=None, modes=("multiclass", "binary"),
                  sample=None) -> int:
    rep = Report()
    guard = leakage_guard or LEAKAGE_GUARD
    band_config = load_band_config()
    print(f"\n=== Factorial v3 preflight ===")
    print(f"data_root={data_root}  stats_dir={stats_dir}  leakage_guard={guard}  "
          f"modes={list(modes)}  sample={sample or 'all'}\n")

    # Authoritative predictor set from the normalization master.
    with open(norm_master) as f:
        master = __import__("json").load(f)
    expected_predictors = master.get("predictor_names", [])
    expected_channels = master.get("in_channels")

    needed_dirs = sorted({FIELD_TEST_DIR} | {d for c in CONFIGS for d in config_patch_dirs(c)})

    # ---[0] Directory presence + counts ---
    print("[0] Directory presence & counts")
    files_by_dir = {}
    for d in needed_dirs:
        dp = data_root / d
        files = sorted(dp.glob("*.tif")) if dp.is_dir() else []
        files_by_dir[d] = files
        rep.check(len(files) > 0, f"dir '{d}'",
                  detail_ok=f"{len(files)} patches",
                  detail_fail=f"missing or empty: {dp}")
    if rep.failures:
        print("\nPREFLIGHT FAILED -- missing patch directories.")
        return 1

    # ---[1] Off-size patches (WARN) ---
    print("\n[1] Off-size patches (dataset skips non-256x256; flagged, not fatal)")
    headers_by_dir = {}
    for d in needed_dirs:
        headers = {f.name: _header(f) for f in tqdm(files_by_dir[d],
                   desc=f"    headers {d}", leave=False, disable=not _SHOW_BARS)}
        headers_by_dir[d] = headers
        off = {name: h["hw"] for name, h in headers.items() if h["hw"] != (PATCH_SIZE, PATCH_SIZE)}
        rep.check(not off, f"patch sizes '{d}'",
                  detail_ok=f"all {len(headers)} are {PATCH_SIZE}x{PATCH_SIZE}",
                  detail_fail=f"{len(off)} off-size (skipped in training): "
                              f"{dict(list(off.items())[:5])}",
                  required=False)

    # ---[2] Predictor parity ---
    print(f"\n[2] Predictor parity (all {len(expected_predictors)} predictors + "
          f"MOD_CLASS, identically named in every dir)")
    for d in needed_dirs:
        missing = {}
        for name, h in headers_by_dir[d].items():
            bs = set(h["bands"])
            miss = [b for b in expected_predictors if b not in bs] + \
                   (["MOD_CLASS"] if "MOD_CLASS" not in bs else [])
            if miss:
                missing[name] = miss
        rep.check(not missing, f"predictor parity '{d}'",
                  detail_ok=f"all {len(expected_predictors)} predictors + MOD_CLASS present",
                  detail_fail=f"{len(missing)} patch(es) missing bands, e.g. "
                              f"{next(iter(missing.items())) if missing else ''}")

    # ---[3] Pairing (field<->NWI) + NWIextra HUC12 subset ---
    print("\n[3] Field<->NWI pairing + NWIextra HUC12 coverage")
    fld_keys = {field_key(f) for f in files_by_dir[FIELD_TEST_DIR]}
    if "R_Patches_NWI" in files_by_dir:
        twin_keys = {nwi_field_twin(f) for f in files_by_dir["R_Patches_NWI"]}
        # REQUIRED direction: every field patch has an NWI twin. The `paired` pool
        # rule maps field train/val basenames -> NWI, so a field patch without a
        # twin silently shrinks the nwi arm's pool relative to fld and confounds
        # the label contrast with training quantity.
        rep.check(fld_keys <= twin_keys, "field<->NWI pairing (every field patch twinned)",
                  detail_ok=f"{len(fld_keys)} field footprints all have NWI twins",
                  detail_fail=f"{len(fld_keys - twin_keys)} field patch(es) lack a twin, "
                              f"e.g. {sorted(fld_keys - twin_keys)[:3]}")
        # NOT required in reverse: NWI patches with no field twin are never
        # selected by `paired` (the map runs field->NWI), so they are inert.
        # Surfaced as a WARN so the dir-count difference is explained, not mysterious.
        orphans = sorted(twin_keys - fld_keys)
        rep.check(not orphans, "NWI orphans (no field twin)",
                  detail_ok="none",
                  detail_fail=f"{len(orphans)} inert NWI patch(es) never selected by "
                              f"the paired rule, e.g. {orphans[:3]}",
                  required=False)
    if "R_Patches_NWIextra" in files_by_dir:
        fld_hucs = {huc12_of(f) for f in files_by_dir[FIELD_TEST_DIR]}
        ext_hucs = {huc12_of(f) for f in files_by_dir["R_Patches_NWIextra"]}
        rep.check(ext_hucs <= fld_hucs, "NWIextra HUC12 subset of field",
                  detail_ok=f"all {len(ext_hucs)} extra HUC12s within field's {len(fld_hucs)}",
                  detail_fail=f"extra-only HUC12s: {sorted(ext_hucs - fld_hucs)[:5]}")

    # ---[4] Footprint identity per paired twin ---
    print("\n[4] Footprint identity per field<->NWI twin (CRS/transform/H/W/nodata)")
    if "R_Patches_NWI" in headers_by_dir:
        fld_h = {field_key(Path(n)): h for n, h in headers_by_dir[FIELD_TEST_DIR].items()}
        nwi_h = {nwi_field_twin(Path(n)): h for n, h in headers_by_dir["R_Patches_NWI"].items()}
        full = (PATCH_SIZE, PATCH_SIZE)
        mism = []
        skipped_off = 0
        for k, fh in fld_h.items():
            nh = nwi_h.get(k)
            if nh is None:
                continue
            if fh["hw"] != full or nh["hw"] != full:
                skipped_off += 1  # off-size already surfaced by [1]; not trainable
                continue
            if not _grid_equal(fh, nh):
                mism.append((k, fh["hw"], nh["hw"]))
        off_note = f" ({skipped_off} off-size twin(s) skipped -- see [1])" if skipped_off else ""
        rep.check(not mism, "field/NWI footprint identity",
                  detail_ok=f"all {len(fld_h) - skipped_off} trainable twins share grid "
                            f"pixel-for-pixel{off_note}",
                  detail_fail=f"{len(mism)} trainable twin(s) differ (key, field_hw, nwi_hw), "
                              f"e.g. {mism[:3]}{off_note}")

    # ---[5] Label values + prevalence ---
    print(f"\n[5] Label values in {{0,1,2,3,255}} + wetland prevalence (sample={sample or 'all'})")
    prevalence = {}
    label_cache = {}  # dir -> {name: array} for sampled patches (reused by [6])
    for d in needed_dirs:
        samp = _sampled(files_by_dir[d], sample)
        counts = np.zeros(256, dtype=np.int64)
        stray = set()
        cache = {}
        for f in tqdm(samp, desc=f"    labels {d}", leave=False, disable=not _SHOW_BARS):
            arr = _read_label(f)
            cache[f.name] = arr
            vals, cnts = np.unique(arr, return_counts=True)
            for v, c in zip(vals, cnts):
                counts[v] += c
                if v not in VALID_LABEL_VALUES:
                    stray.add(int(v))
        label_cache[d] = cache
        rep.check(not stray, f"label values '{d}'",
                  detail_ok=f"only {sorted(int(v) for v in np.nonzero(counts)[0])}",
                  detail_fail=f"stray values {sorted(stray)}")
        labeled = counts[[0, 1, 2, 3]].sum()
        if labeled:
            wet = round(float(counts[[0, 1, 2]].sum() / labeled), 4)
            prevalence[d] = wet
            print(f"      {d}: wetland prevalence={wet}")
    if FIELD_TEST_DIR in prevalence and "R_Patches_NWI" in prevalence:
        print(f"      -> flddeg target: degrade field {prevalence[FIELD_TEST_DIR]} "
              f"down to NWI {prevalence['R_Patches_NWI']} "
              f"({'OK, field>NWI' if prevalence[FIELD_TEST_DIR] > prevalence['R_Patches_NWI'] else 'WARN: field<=NWI, degrade infeasible'})")

    # ---[6] 255-mask identity per twin (reuse sampled label reads) ---
    print("\n[6] NWI 255-mask == field 255-mask per twin (Decision 4.2)")
    fld_cache = label_cache.get(FIELD_TEST_DIR, {})
    nwi_files = {nwi_field_twin(f): f for f in files_by_dir.get("R_Patches_NWI", [])}
    mask_mism = []
    checked = 0
    for fname, farr in fld_cache.items():
        nf = nwi_files.get(field_key(Path(fname)))
        if nf is None:
            continue
        narr = _read_label(nf)
        if farr.shape != narr.shape:
            mask_mism.append(f"{fname} (shape {farr.shape} vs {narr.shape})")
        elif not np.array_equal(farr == IGNORE_VALUE, narr == IGNORE_VALUE):
            mask_mism.append(fname)
        checked += 1
    rep.check(not mask_mism, "fld/NWI 255-mask identity",
              detail_ok=f"identical no-data mask in all {checked} checked twins",
              detail_fail=f"{len(mask_mism)} mismatch(es), e.g. {mask_mism[:3]}")

    # ---[7] Field-anchored split + leakage gate ---
    print(f"\n[7] Field-anchored split + leakage gate (guard={guard}, seeds={list(seeds)})")
    split_ok = True
    detail = ""
    try:
        for seed in seeds:
            tests = []
            for c in CONFIGS:
                _, _, te = P.resolve_pools(c, seed, leakage_guard=guard, data_root=data_root)
                tests.append({f.name for f in te})
            if any(t != tests[0] for t in tests):
                split_ok = False
                detail = f"seed {seed}: test set differs across configs"
                break
            # test must be field only
            if any(not (data_root / FIELD_TEST_DIR / n).exists() for n in list(tests[0])[:20]):
                split_ok = False
                detail = f"seed {seed}: test patches not all from {FIELD_TEST_DIR}"
                break
            if seed == seeds[0]:
                detail = f"seed {seeds[0]}: test={len(tests[0])} field patches, identical across all 8 configs"
    except AssertionError as e:
        split_ok = False
        detail = f"leakage guard tripped: {e}"
    rep.check(split_ok, "split + leakage gate",
              detail_ok=detail, detail_fail=detail)

    # ---[8] Channel counts + stats file presence ---
    print("\n[8] Per-config channels + per-mode stats files")
    for name, cfg in CONFIGS.items():
        n = compute_in_channels(config_bands(cfg), band_config)
        rep.check(n == cfg["channels"], f"channels '{name}'",
                  detail_ok=f"{n} (label={cfg['label']})",
                  detail_fail=f"got {n}, expected {cfg['channels']}")
    if expected_channels is not None:
        # Anchored on the registry, not a literal: v3's full set is 29 (v2 was 26).
        full_set = CONFIGS["fld_chmret_leafoff"]["channels"]
        rep.check(expected_channels == full_set, "norm-master in_channels",
                  detail_ok=f"{full_set} (full feature set)",
                  detail_fail=f"{expected_channels} in {norm_master.name}, "
                              f"expected {full_set}")
    missing_stats = []
    for name in CONFIGS:
        for mode in modes:
            if not (stats_dir / stats_basename(name, mode=mode)).exists():
                missing_stats.append(stats_basename(name, mode=mode))
    rep.check(not missing_stats, "per-config stats files present",
              detail_ok=f"all {len(CONFIGS) * len(modes)} config x mode stats found",
              detail_fail=f"{len(missing_stats)} missing, e.g. {missing_stats[:3]} "
                          f"(run dl_make_config_stats.py --all --mode <mode>)")

    # ---[9] Fusion branch partition (arch_fusion/PLAN.md Section 4.3) ---
    # The guard against the one silent failure mode in --arch mbfusion: a wrong
    # branch->channel map trains fine and reports plausible metrics while feeding
    # each encoder the wrong bands. Validated here, on CPU, before any GPU time.
    #
    # Checked against the STATS FILE's predictor_names, not config_bands(), because
    # the stats file is what WetlandPatchDataset actually indexes the raster by --
    # so this also catches a stats/registry drift that config_bands alone cannot.
    print("\n[9] Fusion branch partition (post-expansion channel space)")
    import json as _json
    for name, cfg in CONFIGS.items():
        stats_p = stats_dir / stats_basename(name, mode=modes[0])
        if not stats_p.exists():
            rep.check(False, f"branch partition '{name}'",
                      detail_fail=f"stats file absent: {stats_p.name}")
            continue
        with open(stats_p) as f:
            preds = _json.load(f)["predictor_names"]
        drift = preds != config_bands(cfg)
        try:
            idx = verify_branch_partition(preds, band_config)
        except (AssertionError, ValueError) as e:
            rep.check(False, f"branch partition '{name}'", detail_fail=str(e))
            continue
        total = sum(len(v) for v in idx.values())
        desc = " ".join(f"{b}:{len(v)}" for b, v in idx.items())
        rep.check(total == cfg["channels"], f"branch partition '{name}'",
                  detail_ok=f"{desc} = {total} ch",
                  detail_fail=f"branches cover {total} ch, config expects {cfg['channels']}")
        rep.check(not drift, f"stats/registry band order '{name}'",
                  detail_ok="stats predictor_names == config_bands()",
                  detail_fail="stats predictor_names differ from config_bands(); "
                              "branch_indices MUST be derived from the stats file "
                              "(re-run dl_make_config_stats.py if unintended)",
                  required=False)

    # --- Summary ---
    print("\n=== Summary ===")
    print(f"  failures (required): {rep.failures}")
    print(f"  warnings (advisory): {rep.warnings}")
    if rep.failures:
        print("\nPREFLIGHT FAILED -- fix the [FAIL] items before GPU time.")
        return 1
    print("\nPREFLIGHT GREEN -- all required checks pass"
          + (" (see WARNs above)." if rep.warnings else "."))
    return 0


def main():
    project_root = Path(__file__).resolve().parents[2]
    data_default = project_root / "Data/Training_Data"
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--data-root", type=Path, default=data_default,
                    help="Dir holding the v2 patch directories")
    ap.add_argument("--stats-dir", type=Path, default=data_default / "stats",
                    help="Dir of per-config stats files")
    ap.add_argument("--norm-master", type=Path,
                    default=data_default / "multiclass_normalization_stats_wp0.5.json",
                    help="Normalization master (authoritative predictor_names / in_channels)")
    ap.add_argument("--seeds", type=int, nargs="+", default=[0, 1, 2])
    ap.add_argument("--leakage-guard", choices=["huc12", "coord"], default=None,
                    help="Split regime to validate (default: dl_experiment_config.LEAKAGE_GUARD)")
    ap.add_argument("--modes", nargs="+", default=["multiclass", "binary"],
                    choices=["multiclass", "binary"])
    ap.add_argument("--sample", type=int, default=None,
                    help="Subsample N patches per dir for the pixel checks [4-6] (default: all)")
    args = ap.parse_args()

    rc = run_preflight(args.data_root, args.stats_dir, args.norm_master,
                       seeds=tuple(args.seeds), leakage_guard=args.leakage_guard,
                       modes=tuple(args.modes), sample=args.sample)
    sys.exit(rc)


if __name__ == "__main__":
    main()
