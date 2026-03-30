"""
dl_band_utils.py

Shared utilities for dynamic band handling across the pipeline.
Reads band names from rasterio descriptions and normalization rules
from dl_band_config.json, eliminating hardcoded band constants.
"""

import json
import rasterio
from pathlib import Path
from typing import Dict, List, Optional


def load_band_config(config_path: Optional[Path] = None) -> dict:
    """
    Load band configuration from dl_band_config.json.

    Args:
        config_path: Path to config file. Defaults to dl_band_config.json
                     in the same directory as this module.

    Returns:
        Configuration dictionary with label_band, default_method,
        band_normalization, class_names, ignore_index.
    """
    if config_path is None:
        config_path = Path(__file__).parent / "dl_band_config.json"

    with open(config_path) as f:
        return json.load(f)


def discover_bands_from_raster(path: Path) -> List[str]:
    """
    Read band names from a raster file's band descriptions.

    Args:
        path: Path to a GeoTIFF file.

    Returns:
        List of band name strings.

    Raises:
        ValueError: If band descriptions are missing or empty.
    """
    with rasterio.open(path) as src:
        descriptions = src.descriptions

    if not descriptions or all(d is None for d in descriptions):
        raise ValueError(
            f"Raster {path} has no band descriptions set. "
            "Band names must be stored in the GeoTIFF band descriptions."
        )

    band_names = [d if d is not None else f"band_{i}" for i, d in enumerate(descriptions)]
    return band_names


def get_predictor_band_names(band_names: List[str], label_band: str) -> List[str]:
    """
    Return band names excluding the label band.

    Args:
        band_names: All band names from the raster.
        label_band: Name of the label band (e.g. "MOD_CLASS").

    Returns:
        List of predictor band names in original order.
    """
    return [name for name in band_names if name != label_band]


def get_normalization_method(band_name: str, config: dict) -> dict:
    """
    Look up the normalization method for a band, with fallback to default.

    Args:
        band_name: Name of the band.
        config: Band configuration dictionary (from load_band_config).

    Returns:
        Dict with at minimum a "method" key (e.g. "min_max", "shift_scale", "one_hot").
    """
    band_norm = config.get("band_normalization", {})
    if band_name in band_norm:
        return band_norm[band_name]
    return {"method": config.get("default_method", "min_max")}


def compute_in_channels(predictor_names: List[str], config: dict) -> int:
    """
    Compute the number of input channels after normalization/one-hot expansion.

    Args:
        predictor_names: List of predictor band names (excluding label).
        config: Band configuration dictionary.

    Returns:
        Total number of input channels for the model.
    """
    count = 0
    for name in predictor_names:
        norm = get_normalization_method(name, config)
        if norm["method"] == "one_hot":
            count += norm["num_classes"]
        else:
            count += 1
    return count


def compute_in_channels_from_stats(stats_path: Path) -> int:
    """
    Read in_channels from a normalization_stats.json file.

    Args:
        stats_path: Path to normalization_stats.json.

    Returns:
        Number of input channels.

    Raises:
        KeyError: If in_channels is not present in the stats file.
    """
    with open(stats_path) as f:
        stats = json.load(f)

    if "in_channels" not in stats:
        raise KeyError(
            f"'in_channels' not found in {stats_path}. "
            "Re-run dl_01_compute_statistics.py to generate an updated stats file."
        )

    return stats["in_channels"]



def validate_prediction_bands(
    raster_bands: List[str],
    expected_predictors: List[str],
    label_band: str
) -> List[int]:
    """
    Match expected predictor bands to raster bands by name.
    Returns the indices into the raster for each expected predictor,
    allowing for reordering and extra bands in the raster.

    Args:
        raster_bands: Band names from the input raster (src.descriptions).
        expected_predictors: Predictor band names the model expects (in order).
        label_band: Name of the label band to exclude.

    Returns:
        List of 1-based band indices into the raster, one per expected predictor.

    Raises:
        ValueError: If any expected predictor band is missing from the raster.
    """
    # Build name->index mapping (1-based for rasterio)
    name_to_idx = {}
    for i, name in enumerate(raster_bands):
        if name != label_band:
            name_to_idx[name] = i + 1  # rasterio uses 1-based indices

    missing = [name for name in expected_predictors if name not in name_to_idx]
    if missing:
        raise ValueError(
            f"Input raster is missing expected predictor bands: {missing}. "
            f"Available bands: {list(name_to_idx.keys())}"
        )

    return [name_to_idx[name] for name in expected_predictors]
