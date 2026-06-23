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

from typing import Dict, List, Optional

from dl_band_utils import load_band_config, compute_in_channels


# --- Band matrix --------------------------------------------------------------

# Constant base present in EVERY config. Geomorph_local one-hot expands 1 -> 10,
# so these 9 names resolve to 18 input channels (8 single + 10 one-hot).
BASE_BANDS: List[str] = [
    "DEM", "slope_local", "Geomorph_local", "flowacc", "twi",
    "r", "g", "b", "nir",
]

# LiDAR structure tier (below-canopy axis). chmret = CHM + return-fraction bands.
LIDAR_TIERS: Dict[str, List[str]] = {
    "nolidar": [],
    "chm":     ["CHM"],
    "chmret":  ["CHM", "pct_below_1m", "pct_1m_to_5m", "pct_above_5m"],
}

# Seasonal-spectral tier (below-canopy axis). leafoff = leaf-off NAIP RGB+NIR.
SPECTRAL_TIERS: Dict[str, List[str]] = {
    "leafon":  [],
    "leafoff": ["r_lo", "g_lo", "b_lo", "nir_lo"],
}

# Resolve a label source to its band name, accepting the first present alias.
# The legacy single MOD_CLASS band IS the field label, so it aliases fld.
LABEL_SOURCE_ALIASES: Dict[str, List[str]] = {
    "fld":    ["MOD_CLASS_FLD", "MOD_CLASS"],
    "nwi":    ["MOD_CLASS_NWI"],
    "flddeg": ["MOD_CLASS_FLDDEG"],
}


# --- The 8 configurations (plan Section 2) ------------------------------------
# `channels` is the expected in_channels from the plan's matrix -- the unit
# check both the preflight and the stats subsetter assert against.

CONFIGS: Dict[str, dict] = {
    "fld_nolidar_leafon":    {"lidar": "nolidar", "spectral": "leafon",  "label": "fld",    "channels": 18},
    "fld_nolidar_leafoff":   {"lidar": "nolidar", "spectral": "leafoff", "label": "fld",    "channels": 22},
    "fld_chm_leafon":        {"lidar": "chm",     "spectral": "leafon",  "label": "fld",    "channels": 19},
    "fld_chm_leafoff":       {"lidar": "chm",     "spectral": "leafoff", "label": "fld",    "channels": 23},
    "fld_chmret_leafon":     {"lidar": "chmret",  "spectral": "leafon",  "label": "fld",    "channels": 22},
    "fld_chmret_leafoff":    {"lidar": "chmret",  "spectral": "leafoff", "label": "fld",    "channels": 26},
    "nwi_chmret_leafoff":    {"lidar": "chmret",  "spectral": "leafoff", "label": "nwi",    "channels": 26},
    "flddeg_chmret_leafoff": {"lidar": "chmret",  "spectral": "leafoff", "label": "flddeg", "channels": 26},
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


if __name__ == "__main__":
    # Quick self-check: print the matrix and assert channel counts.
    bc = load_band_config()
    print(f"{'config':24s} {'label':7s} {'bands':>5s} {'channels':>8s}")
    for name, cfg in CONFIGS.items():
        bands = config_bands(cfg)
        n = config_in_channels(cfg, bc)
        flag = "OK" if n == cfg["channels"] else f"!! expected {cfg['channels']}"
        print(f"{name:24s} {cfg['label']:7s} {len(bands):5d} {n:8d}  {flag}")
    verify_channel_matrix(bc)
    print("\nAll channel counts match the plan matrix.")
