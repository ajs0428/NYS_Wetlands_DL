"""
dl_01_compute_statistics.py

Scan all training patches to compute:
- Per-band min/max for normalization
- Class pixel counts for inverse frequency weighting
- Geomorph class verification

Band names are discovered from rasterio descriptions at runtime.
Normalization methods are read from dl_band_config.json.

Outputs: Data/Training_Data/normalization_stats.json
"""

import json
import numpy as np
import rasterio
from pathlib import Path
from collections import defaultdict
from tqdm import tqdm

from dl_band_utils import (
    load_band_config,
    discover_bands_from_raster,
    get_predictor_band_names,
    get_normalization_method,
    compute_in_channels,
)


def compute_statistics(patches_dir: Path, output_path: Path, config_path: Path = None):
    """
    Compute normalization statistics from all training patches.

    Args:
        patches_dir: Directory containing GeoTIFF patches
        output_path: Path to save JSON statistics file
        config_path: Path to dl_band_config.json (default: alongside this script)
    """
    config = load_band_config(config_path)
    label_band = config["label_band"]
    original_class_names = config["class_names"]
    ignore_index = config["ignore_index"]
    classification_mode = config.get("classification_mode", "multiclass")

    patch_files = sorted(patches_dir.glob("*.tif"))

    if not patch_files:
        raise ValueError(f"No .tif files found in {patches_dir}")

    print(f"Found {len(patch_files)} patch files")

    # Discover band names from first patch
    band_names = discover_bands_from_raster(patch_files[0])
    expected_band_count = len(band_names)
    predictor_names = get_predictor_band_names(band_names, label_band)

    if label_band not in band_names:
        raise ValueError(
            f"Label band '{label_band}' not found in raster descriptions: {band_names}"
        )

    label_index = band_names.index(label_band)

    print(f"Discovered {expected_band_count} bands: {band_names}")
    print(f"Predictor bands: {len(predictor_names)}")
    print(f"Label band: '{label_band}' at index {label_index}")

    # Initialize tracking variables
    band_mins = {name: float('inf') for name in predictor_names}
    band_maxs = {name: float('-inf') for name in predictor_names}
    band_sums = {name: 0.0 for name in predictor_names}
    band_sq_sums = {name: 0.0 for name in predictor_names}
    band_counts = {name: 0 for name in predictor_names}

    geomorph_classes = set()
    class_counts = defaultdict(int)
    total_pixels = 0
    nodata_pixels = 0

    # Identify which predictor bands are one-hot (for geomorph class tracking)
    one_hot_bands = {
        name for name in predictor_names
        if get_normalization_method(name, config)["method"] == "one_hot"
    }

    # Process each patch
    for patch_file in tqdm(patch_files, desc="Processing patches"):
        with rasterio.open(patch_file) as src:
            data = src.read()
            nodata = src.nodata

            # Verify band count
            if data.shape[0] != expected_band_count:
                print(f"Warning: {patch_file.name} has {data.shape[0]} bands, expected {expected_band_count}")
                continue

            # Process each predictor band
            for band_name in predictor_names:
                i = band_names.index(band_name)
                band_data = data[i]

                # Create valid mask (exclude nodata)
                valid_mask = ~np.isnan(band_data)
                if nodata is not None and not np.isnan(nodata):
                    valid_mask &= (band_data != nodata)

                valid_data = band_data[valid_mask]

                if len(valid_data) > 0:
                    band_mins[band_name] = min(band_mins[band_name], float(np.min(valid_data)))
                    band_maxs[band_name] = max(band_maxs[band_name], float(np.max(valid_data)))
                    band_sums[band_name] += float(np.sum(valid_data))
                    band_sq_sums[band_name] += float(np.sum(valid_data ** 2))
                    band_counts[band_name] += len(valid_data)

                    # Track categorical classes
                    if band_name in one_hot_bands:
                        geomorph_classes.update(np.unique(valid_data).astype(int).tolist())

            # Process label band
            label_data = data[label_index]

            # Count NaN pixels as unlabeled (excluded from training via ignore_index)
            nan_mask = np.isnan(label_data)
            nan_count = int(nan_mask.sum())
            if nan_count > 0:
                nodata_pixels += nan_count

            # Count valid class pixels (only labeled classes)
            valid_labels = label_data[~nan_mask]
            for pixel_val in np.unique(valid_labels):
                count = int(np.sum(valid_labels == pixel_val))
                class_counts[int(pixel_val)] += count

            total_pixels += label_data.size

    # Build label remap for binary mode
    label_remap = None
    if classification_mode == "binary":
        binary_mapping = config.get("binary_mapping", {})
        if not binary_mapping:
            raise ValueError("classification_mode is 'binary' but no binary_mapping defined in dl_band_config.json")

        # Map original class names -> original integer indices
        orig_name_to_idx = {name: i for i, name in enumerate(original_class_names)}

        # Build remap: original class index -> binary class index
        binary_class_names = list(binary_mapping.keys())
        label_remap = {}
        for binary_idx, binary_name in enumerate(binary_class_names):
            for orig_name in binary_mapping[binary_name]:
                if orig_name in orig_name_to_idx:
                    label_remap[orig_name_to_idx[orig_name]] = binary_idx

        # Aggregate class counts under binary labels
        remapped_counts = defaultdict(int)
        for orig_idx, count in class_counts.items():
            if orig_idx in label_remap:
                remapped_counts[label_remap[orig_idx]] += count
        class_counts = remapped_counts
        class_names = binary_class_names
        print(f"\nBinary mode: remapping {original_class_names} -> {class_names}")
        print(f"  Label remap: {label_remap}")
    else:
        class_names = original_class_names

    # Compute means and stds
    band_means = {}
    band_stds = {}
    for name in predictor_names:
        if band_counts[name] > 0:
            mean = band_sums[name] / band_counts[name]
            variance = (band_sq_sums[name] / band_counts[name]) - (mean ** 2)
            band_means[name] = float(mean)
            band_stds[name] = float(np.sqrt(max(0, variance)))
        else:
            band_means[name] = 0.0
            band_stds[name] = 1.0

    # Compute class weights (inverse frequency)
    total_class_pixels = sum(class_counts.values())
    class_frequencies = {}
    if total_class_pixels > 0:
        class_frequencies = {k: v / total_class_pixels for k, v in class_counts.items()}

    # Inverse frequency weighting
    class_weights = {}
    for class_idx in range(len(class_names)):
        freq = class_frequencies.get(class_idx, 0)
        if freq > 0:
            class_weights[class_idx] = 1.0 / freq
        else:
            class_weights[class_idx] = 0.0

    # Normalize weights so minimum non-zero weight is 1.0
    non_zero_weights = [w for w in class_weights.values() if w > 0]
    if non_zero_weights:
        min_weight = min(non_zero_weights)
        class_weights = {k: v / min_weight if v > 0 else 0.0 for k, v in class_weights.items()}

    # Build normalization config
    normalization = {}
    for band_name in predictor_names:
        norm_method = get_normalization_method(band_name, config)
        method = norm_method["method"]

        if method == "shift_scale":
            normalization[band_name] = {
                "method": "shift_scale",
                "shift": norm_method["shift"],
                "scale": norm_method["scale"],
                "note": "Maps [-1, 1] to [0, 1]"
            }
        elif method == "one_hot":
            normalization[band_name] = {
                "method": "one_hot",
                "num_classes": norm_method["num_classes"],
                "class_range": norm_method["class_range"],
                "note": f"One-hot encode to {norm_method['num_classes']} channels"
            }
        else:
            normalization[band_name] = {
                "method": "min_max",
                "min": band_mins[band_name],
                "max": band_maxs[band_name],
                "note": "Maps to [0, 1]"
            }

    # Compute in_channels after normalization/one-hot expansion
    in_channels = compute_in_channels(predictor_names, config)

    # Compile statistics
    stats = {
        "num_patches": len(patch_files),
        "total_pixels": total_pixels,
        "labeled_pixels": total_class_pixels,
        "unlabeled_pixels": nodata_pixels,
        "ignore_index": ignore_index,
        "classification_mode": classification_mode,
        "label_remap": {str(k): v for k, v in label_remap.items()} if label_remap else None,
        "label_band": label_band,
        "in_channels": in_channels,
        "band_names": band_names,
        "predictor_names": predictor_names,
        "normalization": normalization,
        "band_statistics": {
            name: {
                "min": band_mins[name],
                "max": band_maxs[name],
                "mean": band_means[name],
                "std": band_stds[name]
            }
            for name in predictor_names
        },
        "geomorph_classes": sorted(list(geomorph_classes)),
        "class_names": class_names,
        "class_counts": {class_names[k]: v for k, v in sorted(class_counts.items()) if k < len(class_names)},
        "class_frequencies": {class_names[k]: round(v, 6) for k, v in sorted(class_frequencies.items()) if k < len(class_names)},
        "class_weights": {class_names[k]: round(v, 4) for k, v in sorted(class_weights.items()) if k < len(class_names)}
    }

    # Save to JSON
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w') as f:
        json.dump(stats, f, indent=2)

    print(f"\nStatistics saved to {output_path}")
    print(f"\nSummary:")
    print(f"  Patches: {len(patch_files)}")
    print(f"  Total pixels: {total_pixels:,}")
    print(f"  Labeled pixels: {total_class_pixels:,}")
    print(f"  Unlabeled pixels (ignore_index={ignore_index}): {nodata_pixels:,}")
    print(f"  Predictor bands: {len(predictor_names)}")
    print(f"  Model input channels: {in_channels}")
    if geomorph_classes:
        print(f"  Categorical classes found: {sorted(list(geomorph_classes))}")
    print(f"\nClass distribution:")
    for class_idx, name in enumerate(class_names):
        count = class_counts.get(class_idx, 0)
        freq = class_frequencies.get(class_idx, 0)
        weight = class_weights.get(class_idx, 0)
        print(f"  {name}: {count:,} pixels ({freq*100:.2f}%) - weight: {weight:.4f}")

    return stats


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Compute normalization statistics from training patches")
    parser.add_argument(
        "--patches-dir",
        type=Path,
        default=Path("Data/Training_Data/R_Patches"),
        help="Directory containing GeoTIFF patches"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("Data/Training_Data/normalization_stats.json"),
        help="Output path for statistics JSON"
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=None,
        help="Path to dl_band_config.json (default: alongside this script)"
    )
    args = parser.parse_args()

    # Handle relative paths from project root
    project_root = Path(__file__).parent.parent.parent
    patches_dir = project_root / args.patches_dir if not args.patches_dir.is_absolute() else args.patches_dir
    output_path = project_root / args.output if not args.output.is_absolute() else args.output

    compute_statistics(patches_dir, output_path, args.config)
