"""
dl_experiment_config.py

Single source of truth for the factorial experiment's band matrix and the 8
configurations (plan Section 2). Imported by:
  - dl_preflight_check.py     (Phase 0 -- validates the data against this matrix)
  - dl_make_config_stats.py   (Phase 1.1/1.4 -- derives per-config stats files)
  - the Phase 2 shell orchestration (band lists per config)

Defining the matrix here keeps band lists, label-source aliases, and the
expected channel counts in exactly one place, so a change to the experiment
design can never drift between the preflight and the stats/runner code.

The experiment holds architecture, loss, optimizer, schedule, and splits
constant; only the active predictor bands and the label source change across
configs (plan Section 1).
"""

import os
import re
from typing import Dict, List, Optional

from dl_band_utils import (
    load_band_config,
    compute_in_channels,
    get_normalization_method,
    stats_filename,
)


# --- Band matrix --------------------------------------------------------------

# Constant base present in EVERY config. Geomorph_local one-hot expands 1 -> 10,
# so these 12 names resolve to 21 input channels (11 single + 10 one-hot).
#
# v3 (2026-08-17) added TPI_local, meanc_local, dmv_local -- three terrain metrics
# produced upstream in NYS_Wetlands_Data (step_terrain.sh /
# terrain_metrics_filter_singleVect_CMD.R). This is what moved every config up by
# 3 channels (v2: 18/22/26 -> v3: 21/25/29) and what makes the arch_fusion
# terrain branch 17 channels wide. The terrain block is listed in raster band
# order; note config_bands() as a whole is NOT raster order (CHM sorts after
# r,g,b,nir here but before them in the raster) -- see branch_indices_from_predictors.
BASE_BANDS: List[str] = [
    "DEM", "slope_local", "TPI_local", "Geomorph_local", "meanc_local", "dmv_local",
    "flowacc", "twi",
    "r", "g", "b", "nir",
]

# LiDAR structure tier (below-canopy axis). v2 contrasts no-structure vs. full
# structure only; the v1 CHM-only tier was dropped (CHM is grouped with the
# return fractions -- see plan §2).
LIDAR_TIERS: Dict[str, List[str]] = {
    "nolidar": [],
    "chmret":  ["CHM", "pct_below_1m", "pct_1m_to_5m", "pct_above_5m"],
}

# Seasonal-spectral tier (below-canopy axis). leafoff = leaf-off NAIP RGB+NIR.
SPECTRAL_TIERS: Dict[str, List[str]] = {
    "leafon":  [],
    "leafoff": ["r_lo", "g_lo", "b_lo", "nir_lo"],
}

# --- Label band names ---------------------------------------------------------
# In v2 each label source lives in its OWN patch directory (see LABEL_SOURCES
# below) and the label band is always "MOD_CLASS" -- provenance is the directory,
# not the band name (verified: R_Patches and R_Patches_NWI each carry a single
# MOD_CLASS band, differing only in what that band means). The v1 merged-file
# band names are kept as LEADING aliases so a v1 scan (dl_make_config_stats /
# preflight against a merged multi-band patch) still resolves; the trailing
# "MOD_CLASS" is the v2 separate-directory case.
LABEL_SOURCE_ALIASES: Dict[str, List[str]] = {
    "fld":      ["MOD_CLASS_FLD", "MOD_CLASS"],
    "nwi":      ["MOD_CLASS_NWI", "MOD_CLASS"],
    "nwiextra": ["MOD_CLASS"],
    "nwifield": ["MOD_CLASS"],
    "flddeg":   ["MOD_CLASS_FLDDEG", "MOD_CLASS"],
}

# --- v2 data-source registry (plan §10.1) -------------------------------------
# Which patch directory(ies) supply TRAIN/VAL for each label source, and the
# pool rule that resolves them against the field-anchored split (plan 4.5-4.6).
# The TEST set is ALWAYS FIELD_TEST_DIR at the seed's test_fld patches, regardless
# of source, so every config is judged on the same gold-standard field pixels.
# Directory names are relative to Data/Training_Data/. Consumed by the Phase 1.2
# resolver (dl_patch_pools.py) and the Phase 2 runner via `--emit`.
#
# Two DIFFERENT identity relations are in play (see the keying helpers below);
# conflating them was the v1->v2 keying bug. A naive "cluster_..._patch_N" key
# collides 56x WITHIN R_Patches and spuriously "matches" 594 geographically
# distinct NWIextra patches to field, because each source dataset numbers patch_N
# in its own namespace:
#   * field<->NWI PAIRING  uses field_key()/nwi_field_twin() -- R_Patches_NWI is
#     exactly "NWI_" + the field basename (a verified 1:1 footprint pairing).
#     NOTE 252 field patches are themselves NWI-sourced and start with "NWI_", so
#     their twin is DOUBLE-prefixed ("NWI_NWI_AJS_..."); the mapping strips
#     exactly ONE leading "NWI_" from an NWI-DIR file and never touches field
#     basenames -- hence the pairing is directory-aware, not a blind strip.
#   * NWIextra LEAKAGE      uses huc12_of() -- NWIextra is NEW ground (0 patch
#     overlap with field, nearest >=258 m) but shares all 29 HUC12s, so the
#     guard is geographic (HUC12 by default), never filename-based.
#
#   pool_rule semantics (implemented in dl_patch_pools.py):
#     anchored      train/val = R_Patches[train_fld / val_fld basenames]     (fld_*)
#     paired        train/val = R_Patches_NWI paired via field_pair_key to
#                               train_fld / val_fld                          (nwi)
#     extra_pool    train/val = (R_Patches_NWI U R_Patches_NWIextra) minus
#                               LEAKAGE_GUARD (HUC12s holding a test_fld patch),
#                               seed-split -> ~2x NWI quantity test          (nwiextra)
#     hybrid_union  train/val = R_Patches[train/val_fld] (field labels)
#                               U R_Patches_NWIextra minus LEAKAGE_GUARD     (nwifield)
#     degrade       train/val = R_Patches[train/val_fld] with seeded
#                               wetland->UPL relabel                         (flddeg)
FIELD_TEST_DIR: str = "R_Patches"

# Geographic guard keeping the NWIextra train pool clear of the field TEST set
# (plan Decision 4.5, resolved: run both, HUC12 as the headline). "huc12" = drop
# any extra patch sharing a HUC12 with a test_fld patch (conservative; guards
# spatial autocorrelation). "coord" = drop only extra patches whose 256 m window
# overlaps a test_fld patch (currently none -> keeps ~all extra; sensitivity
# run). dl_patch_pools.py honors this default; a --leakage-guard CLI overrides.
LEAKAGE_GUARD: str = "huc12"

LABEL_SOURCES: Dict[str, dict] = {
    "fld":      {"patch_dirs": ["R_Patches"],                        "pool_rule": "anchored"},
    "nwi":      {"patch_dirs": ["R_Patches_NWI"],                    "pool_rule": "paired"},
    "nwiextra": {"patch_dirs": ["R_Patches_NWI", "R_Patches_NWIextra"], "pool_rule": "extra_pool"},
    "nwifield": {"patch_dirs": ["R_Patches", "R_Patches_NWIextra"],  "pool_rule": "hybrid_union"},
    "flddeg":   {"patch_dirs": ["R_Patches"],                        "pool_rule": "degrade"},
}

# --- Patch identity helpers (v2) ----------------------------------------------
# Do NOT key patches by the "cluster_..._patch_N" substring: it drops the source
# prefix (ADK_WCT_AJS_, gps_jc_, NWI_RSM_, NEW_AJS_, ...), which is identity-
# bearing -- every source restarts patch_N within a cluster+HUC, so the substring
# is neither unique within a directory (56 collisions in R_Patches) nor
# geographically meaningful across directories (594 false field<->NWIextra hits).
_NWI_PREFIX_RE = re.compile(r"^NWI_")
_HUC12_RE = re.compile(r"huc_(\d+)")


# --- The 8 configurations (plan Section 2) ------------------------------------
# `channels` is the expected in_channels from the plan's matrix -- the unit
# check both the preflight and the stats subsetter assert against.

CONFIGS: Dict[str, dict] = {
    # Field-verified feature factorial (2 LiDAR x 2 spectral)
    "fld_nolidar_leafon":     {"lidar": "nolidar", "spectral": "leafon",  "label": "fld",      "channels": 21},
    "fld_nolidar_leafoff":    {"lidar": "nolidar", "spectral": "leafoff", "label": "fld",      "channels": 25},
    "fld_chmret_leafon":      {"lidar": "chmret",  "spectral": "leafon",  "label": "fld",      "channels": 25},
    "fld_chmret_leafoff":     {"lidar": "chmret",  "spectral": "leafoff", "label": "fld",      "channels": 29},  # full feature set / channel anchor
    # Label block (full feature set only -- scope control)
    "nwi_chmret_leafoff":     {"lidar": "chmret",  "spectral": "leafoff", "label": "nwi",      "channels": 29},
    "nwiextra_chmret_leafoff":{"lidar": "chmret",  "spectral": "leafoff", "label": "nwiextra", "channels": 29},
    "nwifield_chmret_leafoff":{"lidar": "chmret",  "spectral": "leafoff", "label": "nwifield", "channels": 29},
    "flddeg_chmret_leafoff":  {"lidar": "chmret",  "spectral": "leafoff", "label": "flddeg",   "channels": 29},
}


# --- Multi-branch fusion partition (arch_fusion/PLAN.md Section 2) -------------
# Which bands feed which encoder branch in `--arch mbfusion`, and how wide each
# branch's encoder is at level 0. Drawn by physical process / sensing modality;
# all inputs are native 1 m (SAR and Sentinel-2 are not in this stack).
#
# Every band in BASE_BANDS + LIDAR_TIERS + SPECTRAL_TIERS must appear in exactly
# one branch -- verify_branch_partition() enforces that, so adding a band to the
# factorial without assigning it to a branch fails loudly here rather than
# silently dropping it from the fusion model's input.
BRANCH_BANDS: Dict[str, List[str]] = {
    "terrain":  ["DEM", "slope_local", "TPI_local", "Geomorph_local",
                 "meanc_local", "dmv_local", "flowacc", "twi"],   # 17 ch (Geomorph 1->10)
    "lidar":    ["CHM", "pct_below_1m", "pct_1m_to_5m", "pct_above_5m"],  # 4 ch
    "leafon":   ["r", "g", "b", "nir"],                                   # 4 ch
    "leafoff":  ["r_lo", "g_lo", "b_lo", "nir_lo"],                       # 4 ch
}

# Encoder width per branch at level 0; level L is width * 2**L. Deliberately NOT
# proportional to channel count: proportional allocation would hand terrain ~59%
# of the width largely because geomorphon happens to be one-hot encoded into 10
# channels carrying ~3.3 bits, starving the LiDAR and leaf-off branches -- the two
# meant to resolve the UPL->FSW confusion. Terrain leads because wetland
# occurrence is terrain-driven; the other three lead the vegetation-class split.
BRANCH_WIDTHS: Dict[str, int] = {"terrain": 48, "lidar": 32, "leafon": 32, "leafoff": 32}


# --- Helpers ------------------------------------------------------------------

def config_bands(cfg: dict) -> List[str]:
    """Active predictor band names for one config, in load order (base, lidar, leaf-off)."""
    return BASE_BANDS + LIDAR_TIERS[cfg["lidar"]] + SPECTRAL_TIERS[cfg["spectral"]]


def resolve_label_band(present_bands, source: str) -> Optional[str]:
    """Return the band name for a label source given the bands present, else None."""
    for alias in LABEL_SOURCE_ALIASES[source]:
        if alias in present_bands:
            return alias
    return None


def field_key(path: str) -> str:
    """Field-space identity of a R_Patches patch: its basename, unchanged.

    This anchors the field split (each of the 689 field basenames is unique) and
    is the target that nwi_field_twin() maps R_Patches_NWI patches onto. Do NOT
    apply this to R_Patches_NWI files -- 252 field patches are NWI-sourced and
    legitimately start with "NWI_", so field identity is the raw basename, never
    a prefix-stripped one.
    """
    return os.path.basename(path)


def nwi_field_twin(path: str) -> str:
    """Map an R_Patches_NWI file to the field basename it pairs with.

    R_Patches_NWI names are exactly "NWI_" + the field basename (verified 1:1),
    so stripping exactly ONE leading "NWI_" recovers the field twin -- including
    the double-prefixed "NWI_NWI_..." twins of the 252 NWI-sourced field patches.
    Only valid on R_Patches_NWI files; for R_Patches use field_key(), and for
    R_Patches_NWIextra (independent patch_N namespace) use huc12_of().
    """
    return _NWI_PREFIX_RE.sub("", os.path.basename(path), count=1)


def huc12_of(path: str) -> str:
    """The 12-digit HUC code in a patch filename (e.g. '042900030101').

    Geographic key for the NWIextra leakage guard (LEAKAGE_GUARD='huc12'):
    NWIextra patches are new footprints but share HUC12s with field, so the guard
    drops any extra patch whose HUC12 holds a field test patch. Raises if the
    filename carries no 'huc_<digits>' token.
    """
    m = _HUC12_RE.search(os.path.basename(path))
    if not m:
        raise ValueError(f"no 'huc_<digits>' token in filename: {os.path.basename(path)!r}")
    return m.group(1)


def config_label_source(name: str) -> str:
    """Label-source key for a config (fld / nwi / nwiextra / nwifield / flddeg)."""
    return get_config(name)["label"]


def config_patch_dirs(name: str) -> List[str]:
    """Patch directory name(s) that supply TRAIN/VAL for a config (plan §10.1)."""
    return LABEL_SOURCES[config_label_source(name)]["patch_dirs"]


def config_pool_rule(name: str) -> str:
    """Pool rule that resolves a config's train/val split (plan 4.5-4.6)."""
    return LABEL_SOURCES[config_label_source(name)]["pool_rule"]


def config_in_channels(cfg: dict, band_config: Optional[dict] = None) -> int:
    """Resolved input-channel count for a config (Geomorph one-hot 1 -> 10)."""
    if band_config is None:
        band_config = load_band_config()
    return compute_in_channels(config_bands(cfg), band_config)


def get_config(name: str) -> dict:
    """Look up a config by name with a helpful error listing valid names."""
    try:
        return CONFIGS[name]
    except KeyError:
        raise KeyError(f"Unknown config '{name}'. Valid: {sorted(CONFIGS)}")


def eval_config_name(name: str) -> str:
    """The config whose stats are used for EVALUATION of `name`.

    Section 3 is non-negotiable: the test set is always field-labeled. So a
    config trains on its own label source but is evaluated with the matching
    field config's stats (same predictors, MOD_CLASS label). For fld configs
    this is the config itself; for nwi/flddeg it is the fld config with the same
    feature set (same lidar + spectral tier).
    """
    cfg = get_config(name)
    return f"fld_{cfg['lidar']}_{cfg['spectral']}"


def stats_basename(name: str, mode: str = "multiclass", weight_power: float = 0.5) -> str:
    """Per-config stats filename, matching dl_make_config_stats's output."""
    base = stats_filename(mode, weight_power)  # e.g. multiclass_normalization_stats_wp0.5.json
    return base.replace("_normalization_stats", f"_normalization_stats_{name}")


# --- Multi-branch fusion helpers (arch_fusion/PLAN.md Section 4) --------------

def _band_to_branch() -> Dict[str, str]:
    """Reverse BRANCH_BANDS into band -> branch, rejecting duplicate assignments."""
    rev: Dict[str, str] = {}
    for branch, bands in BRANCH_BANDS.items():
        for b in bands:
            if b in rev:
                raise ValueError(
                    f"band '{b}' assigned to two branches: '{rev[b]}' and '{branch}'")
            rev[b] = branch
    return rev


def branch_indices_from_predictors(
    predictor_names: List[str],
    band_config: Optional[dict] = None,
) -> Dict[str, List[int]]:
    """Map each fusion branch to its channel indices in the model's input tensor.

    THE ONE THING THAT MUST BE RIGHT (PLAN Section 4.1). Indices are in
    POST-EXPANSION channel space: Geomorph_local occupies 10 contiguous channels,
    not 1, because dl_02_dataset.normalize_and_expand() one-hots it before the
    tensor reaches the model.

    `predictor_names` must be the dataset's channel order -- i.e.
    stats["predictor_names"], which dl_make_config_stats writes as
    config_bands(cfg) = BASE + LIDAR + SPECTRAL. It is NOT raster band order
    (dl_02 re-indexes the raster by name), and it is NOT BRANCH_BANDS order.
    Passing the wrong list here mis-slices silently: the model trains fine and
    reports plausible metrics while reading the wrong bands.

    Branches whose bands the active config does not supply are omitted entirely,
    so a `nolidar` or `leafon` config yields 3 branches and the fusion gate
    softmaxes over 3 rather than erroring.

    Args:
        predictor_names: Ordered predictor band names (stats["predictor_names"]).
        band_config: dl_band_config.json contents; loaded if None.

    Returns:
        {branch: [channel indices]}, in BRANCH_BANDS declaration order, omitting
        branches with no bands present. Indices within a branch are ascending.

    Raises:
        ValueError: If a predictor belongs to no branch.
    """
    if band_config is None:
        band_config = load_band_config()

    rev = _band_to_branch()
    out: Dict[str, List[int]] = {b: [] for b in BRANCH_BANDS}

    offset = 0
    for name in predictor_names:
        norm = get_normalization_method(name, band_config)
        width = norm["num_classes"] if norm["method"] == "one_hot" else 1
        branch = rev.get(name)
        if branch is None:
            raise ValueError(
                f"predictor '{name}' is not assigned to any fusion branch. "
                f"Add it to BRANCH_BANDS (branches: {sorted(BRANCH_BANDS)})."
            )
        out[branch].extend(range(offset, offset + width))
        offset += width

    return {b: idx for b, idx in out.items() if idx}


def branch_widths_for(branch_indices: Dict[str, List[int]]) -> Dict[str, int]:
    """Level-0 encoder width per PRESENT branch, matching branch_indices' keys.

    Serialized into the checkpoint alongside branch_indices (PLAN Section 4.2):
    BRANCH_WIDTHS being a module constant is not a reason to skip it, since an
    equal-width control arm would otherwise be indistinguishable after the fact.
    """
    return {b: BRANCH_WIDTHS[b] for b in branch_indices}


def verify_branch_partition(
    predictor_names: List[str],
    band_config: Optional[dict] = None,
) -> Dict[str, List[int]]:
    """Assert the branch slices exactly partition the input tensor.

    The CPU-side guard from PLAN Section 4.3, called by dl_preflight_check before
    any GPU time and again by the model at construction. Checks that the branch
    index sets are disjoint, that their union is exactly range(in_channels), and
    that Geomorph_local's one-hot block is contiguous and inside the terrain
    branch.

    Returns the validated branch_indices so callers can use it directly.
    """
    if band_config is None:
        band_config = load_band_config()

    idx = branch_indices_from_predictors(predictor_names, band_config)
    total = compute_in_channels(predictor_names, band_config)

    flat = [i for v in idx.values() for i in v]
    if len(flat) != len(set(flat)):
        dupes = sorted({i for i in flat if flat.count(i) > 1})
        raise AssertionError(f"branch slices overlap at channel(s) {dupes}")
    if set(flat) != set(range(total)):
        missing = sorted(set(range(total)) - set(flat))
        extra = sorted(set(flat) - set(range(total)))
        raise AssertionError(
            f"branch slices do not cover the input: missing={missing} extra={extra} "
            f"(in_channels={total})"
        )

    # Geomorph one-hot block: contiguous, correct width, and in the terrain branch.
    onehot = [n for n in predictor_names
              if get_normalization_method(n, band_config)["method"] == "one_hot"]
    for name in onehot:
        width = get_normalization_method(name, band_config)["num_classes"]
        start = compute_in_channels(predictor_names[:predictor_names.index(name)], band_config)
        block = list(range(start, start + width))
        branch = _band_to_branch()[name]
        if not set(block).issubset(idx.get(branch, [])):
            raise AssertionError(
                f"one-hot band '{name}' block {block[0]}..{block[-1]} is not fully "
                f"inside branch '{branch}'"
            )

    return idx


def config_branch_indices(name: str, band_config: Optional[dict] = None) -> Dict[str, List[int]]:
    """branch_indices for a config, derived from config_bands().

    Convenience for preflight and self-checks. TRAINING must instead derive the
    map from the loaded stats file's predictor_names -- the two agree only so long
    as dl_make_config_stats keeps writing predictor_names = config_bands(cfg),
    and the stats file is what the dataset actually indexes by.
    """
    return verify_branch_partition(config_bands(get_config(name)), band_config)


def verify_channel_matrix(band_config: Optional[dict] = None) -> None:
    """Assert every config resolves to its plan-specified channel count.

    Cheap invariant the preflight and stats subsetter both lean on; raises
    AssertionError on the first mismatch so a band-list edit can't silently
    change channel counts.
    """
    if band_config is None:
        band_config = load_band_config()
    for name, cfg in CONFIGS.items():
        n = config_in_channels(cfg, band_config)
        assert n == cfg["channels"], (
            f"config '{name}': resolved {n} channels, plan expects {cfg['channels']}"
        )


def _emit(name: str, mode: str = "multiclass") -> None:
    """Print shell-sourceable vars for one config (used by run_config.sh).

    Values are quoted so multi-word fields (PATCH_DIRS for the hybrid config)
    survive `eval` in the shell runner.
    """
    cfg = get_config(name)
    ev = eval_config_name(name)
    bc = load_band_config()
    vals = {
        "CONFIG": name,
        "MODE": mode,
        "LABEL_SOURCE": cfg["label"],
        "FEATURESET": f"{cfg['lidar']}_{cfg['spectral']}",
        "IN_CHANNELS": config_in_channels(cfg, bc),
        "PATCH_DIRS": " ".join(config_patch_dirs(name)),
        "POOL_RULE": config_pool_rule(name),
        "FIELD_TEST_DIR": FIELD_TEST_DIR,
        "TRAIN_STATS": stats_basename(name, mode=mode),
        "EVAL_CONFIG": ev,
        "EVAL_STATS": stats_basename(ev, mode=mode),
    }
    for k, v in vals.items():
        print(f'{k}="{v}"')


if __name__ == "__main__":
    import argparse
    ap = argparse.ArgumentParser(description="Factorial experiment band matrix / config registry")
    ap.add_argument("--emit", metavar="CONFIG",
                    help="Print shell-sourceable vars for CONFIG (TRAIN_STATS/EVAL_STATS/...)")
    ap.add_argument("--mode", default="multiclass", choices=["multiclass", "binary"],
                    help="Classification mode for stats filenames in --emit (default: multiclass)")
    ap.add_argument("--list", action="store_true", help="Print all config names (one per line)")
    args = ap.parse_args()

    if args.list:
        print("\n".join(CONFIGS))
    elif args.emit:
        _emit(args.emit, mode=args.mode)
    else:
        # Self-check: print the matrix and assert channel counts.
        bc = load_band_config()
        print(f"{'config':24s} {'label':7s} {'eval-stats config':20s} {'bands':>5s} {'channels':>8s}")
        for name, cfg in CONFIGS.items():
            n = config_in_channels(cfg, bc)
            flag = "OK" if n == cfg["channels"] else f"!! expected {cfg['channels']}"
            print(f"{name:24s} {cfg['label']:7s} {eval_config_name(name):20s} "
                  f"{len(config_bands(cfg)):5d} {n:8d}  {flag}")
        verify_channel_matrix(bc)
        print("\nAll channel counts match the plan matrix.")

        # Branch partition (arch_fusion): every config must slice cleanly.
        print(f"\n{'config':24s} {'branches (channels)':52s} {'total':>5s}")
        for name in CONFIGS:
            idx = config_branch_indices(name, bc)
            desc = "  ".join(f"{b}:{len(v)}" for b, v in idx.items())
            print(f"{name:24s} {desc:52s} {sum(len(v) for v in idx.values()):5d}")
        print("\nAll configs partition cleanly into fusion branches.")
