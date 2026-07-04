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

from dl_band_utils import load_band_config, compute_in_channels, stats_filename


# --- Band matrix --------------------------------------------------------------

# Constant base present in EVERY config. Geomorph_local one-hot expands 1 -> 10,
# so these 9 names resolve to 18 input channels (8 single + 10 one-hot).
BASE_BANDS: List[str] = [
    "DEM", "slope_local", "Geomorph_local", "flowacc", "twi",
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
# The TEST set is ALWAYS FIELD_TEST_DIR at the seed's test_fld keys, regardless
# of source, so every config is judged on the same gold-standard field pixels.
# Directory names are relative to Data/Training_Data/. Consumed by the Phase 1.2
# resolver (dl_patch_pools.py) and the Phase 2 runner via `--emit`.
#
#   pool_rule semantics (implemented in dl_patch_pools.py):
#     anchored      train/val = <dir>[train_fld / val_fld keys]              (fld_*)
#     paired        train/val = R_Patches_NWI[train_fld / val_fld keys]      (nwi)
#     extra_pool    train/val = R_Patches_NWIextra[keys not in test_fld],
#                               seed-split                                   (nwiextra)
#     hybrid_union  train/val = R_Patches[train/val_fld] (field labels)
#                               U R_Patches_NWIextra[extra keys, not test]   (nwifield)
#     degrade       train/val = R_Patches[train/val_fld] with seeded
#                               wetland->UPL relabel                         (flddeg)
FIELD_TEST_DIR: str = "R_Patches"

LABEL_SOURCES: Dict[str, dict] = {
    "fld":      {"patch_dirs": ["R_Patches"],                        "pool_rule": "anchored"},
    "nwi":      {"patch_dirs": ["R_Patches_NWI"],                    "pool_rule": "paired"},
    "nwiextra": {"patch_dirs": ["R_Patches_NWIextra"],               "pool_rule": "extra_pool"},
    "nwifield": {"patch_dirs": ["R_Patches", "R_Patches_NWIextra"],  "pool_rule": "hybrid_union"},
    "flddeg":   {"patch_dirs": ["R_Patches"],                        "pool_rule": "degrade"},
}

# Directory-independent patch identity: the filename from "cluster_" onward.
_LOCATION_KEY_RE = re.compile(r"cluster_.*$")


# --- The 8 configurations (plan Section 2) ------------------------------------
# `channels` is the expected in_channels from the plan's matrix -- the unit
# check both the preflight and the stats subsetter assert against.

CONFIGS: Dict[str, dict] = {
    # Field-verified feature factorial (2 LiDAR x 2 spectral)
    "fld_nolidar_leafon":     {"lidar": "nolidar", "spectral": "leafon",  "label": "fld",      "channels": 18},
    "fld_nolidar_leafoff":    {"lidar": "nolidar", "spectral": "leafoff", "label": "fld",      "channels": 22},
    "fld_chmret_leafon":      {"lidar": "chmret",  "spectral": "leafon",  "label": "fld",      "channels": 22},
    "fld_chmret_leafoff":     {"lidar": "chmret",  "spectral": "leafoff", "label": "fld",      "channels": 26},  # full feature set / channel anchor
    # Label block (full feature set only -- scope control)
    "nwi_chmret_leafoff":     {"lidar": "chmret",  "spectral": "leafoff", "label": "nwi",      "channels": 26},
    "nwiextra_chmret_leafoff":{"lidar": "chmret",  "spectral": "leafoff", "label": "nwiextra", "channels": 26},
    "nwifield_chmret_leafoff":{"lidar": "chmret",  "spectral": "leafoff", "label": "nwifield", "channels": 26},
    "flddeg_chmret_leafoff":  {"lidar": "chmret",  "spectral": "leafoff", "label": "flddeg",   "channels": 26},
}


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


def location_key(path: str) -> str:
    """Directory-independent patch identity: the filename from 'cluster_' onward.

    Field and NWI copies of the same footprint differ only by a leading source
    prefix -- e.g. 'NWI_ADK_WCT_AJS_cluster_11_..._patch_10_256m.tif' vs
    'ADK_WCT_AJS_cluster_11_..._patch_10_256m.tif' -- so the substring from
    'cluster_' to the end uniquely keys a ground footprint across directories
    (plan Decision 4.1 / 4.5). Raises if the naming convention is not met.
    """
    name = os.path.basename(path)
    m = _LOCATION_KEY_RE.search(name)
    if not m:
        raise ValueError(f"no 'cluster_...' location key in filename: {name!r}")
    return m.group(0)


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
