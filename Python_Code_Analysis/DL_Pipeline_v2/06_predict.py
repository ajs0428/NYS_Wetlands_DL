"""
06_predict.py

Apply trained model to new rasters for inference.
Handles full rasters with sliding window and outputs GeoTIFFs.

Band names are matched by name from rasterio descriptions,
so prediction rasters can have bands in any order or extra bands.
"""

import json
import numpy as np
import rasterio
from rasterio.windows import Window
import torch
import torch.nn as nn
from pathlib import Path
from typing import Optional, Tuple, List
import importlib.util
import sys
from tqdm import tqdm

# Import from sibling modules
def _import_module(name, path):
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module

_script_dir = Path(__file__).parent
_model = _import_module("unet_model", _script_dir / "03_unet_model.py")
_dataset = _import_module("dataset", _script_dir / "02_dataset.py")

from band_utils import (
    discover_bands_from_raster,
    validate_prediction_bands,
)
from model_utils import load_model

normalize_bands = _dataset.normalize_bands
get_device = _model.get_device


def load_normalization_stats(stats_path: Path) -> dict:
    """Load normalization statistics from JSON."""
    with open(stats_path) as f:
        return json.load(f)


@torch.no_grad()
def predict_raster(
    model: nn.Module,
    input_path: Path,
    output_path: Path,
    stats: dict,
    device: torch.device,
    patch_size: int = 128,
    overlap: int = 64,
    save_probabilities: bool = False
):
    """
    Apply model to a full raster using sliding window.

    Args:
        model: Trained model
        input_path: Path to input raster (predictor bands, with or without label band)
        output_path: Path for output classification raster
        stats: Normalization statistics
        device: Computation device
        patch_size: Size of sliding window
        overlap: Overlap between windows for smoother predictions
        save_probabilities: Whether to save per-class probability maps
    """
    predictor_names = stats["predictor_names"]
    label_band = stats["label_band"]
    class_names = stats["class_names"]
    num_classes = len(class_names)

    with rasterio.open(input_path) as src:
        profile = src.profile.copy()
        height = src.height
        width = src.width
        nodata = src.nodata

        # Match bands by name
        raster_bands = list(src.descriptions)
        band_indices = validate_prediction_bands(raster_bands, predictor_names, label_band)

        # Read predictor bands in the correct order
        data = src.read(band_indices).astype(np.float32)

    print(f"Input raster: {width} x {height} pixels")
    print(f"Matched {len(predictor_names)} predictor bands by name")
    print(f"Patch size: {patch_size}, Overlap: {overlap}")

    # Initialize output arrays — always accumulate in probability space
    probabilities = np.zeros((num_classes, height, width), dtype=np.float32)
    counts = np.zeros((height, width), dtype=np.float32)

    # Calculate step size
    step = patch_size - overlap

    # Build 2D Hanning blending kernel: smooth taper from 1.0 at center
    # to ~0 at edges, eliminating hard tile boundaries
    hann_1d = np.hanning(patch_size).astype(np.float32)
    weight = np.outer(hann_1d, hann_1d)
    # Clamp minimum so pixels still contribute even at corners
    weight = np.clip(weight, 1e-3, None)

    # Generate window positions
    y_positions = list(range(0, height - patch_size + 1, step))
    x_positions = list(range(0, width - patch_size + 1, step))

    # Add final row/column if needed
    if y_positions[-1] + patch_size < height:
        y_positions.append(height - patch_size)
    if x_positions[-1] + patch_size < width:
        x_positions.append(width - patch_size)

    total_patches = len(y_positions) * len(x_positions)
    print(f"Processing {total_patches} patches...")

    # Process patches
    for y in tqdm(y_positions, desc="Rows"):
        for x in x_positions:
            # Extract patch
            patch = data[:, y:y+patch_size, x:x+patch_size]

            # Normalize
            normalized = normalize_bands(patch, stats["normalization"], predictor_names, nodata)

            # Convert to tensor and predict
            tensor = torch.from_numpy(normalized).unsqueeze(0).to(device)
            output = model(tensor)
            probs = torch.softmax(output, dim=1).squeeze(0).cpu().numpy()

            # Accumulate weighted probabilities
            probabilities[:, y:y+patch_size, x:x+patch_size] += probs * weight
            counts[y:y+patch_size, x:x+patch_size] += weight

    # Normalize accumulated probabilities by total weight
    valid = counts > 0
    probabilities[:, valid] /= counts[valid]

    # Derive class predictions from blended probabilities
    predictions = np.where(valid, probabilities.argmax(axis=0).astype(np.uint8), 255)

    # Write output
    profile.update(
        count=1,
        dtype='uint8',
        nodata=255
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)

    with rasterio.open(output_path, 'w', **profile) as dst:
        dst.write(predictions, 1)
        dst.set_band_description(1, "Predicted Class")

    print(f"Classification saved to {output_path}")

    # Save probability maps if requested
    if save_probabilities:
        prob_path = output_path.with_suffix('.probs.tif')
        prob_profile = profile.copy()
        prob_profile.update(
            count=num_classes,
            dtype='float32',
            nodata=-1
        )

        # Zero out probabilities where no predictions were made
        probabilities[:, ~valid] = -1

        with rasterio.open(prob_path, 'w', **prob_profile) as dst:
            for i in range(num_classes):
                dst.write(probabilities[i], i + 1)
                dst.set_band_description(i + 1, f"{class_names[i]} Probability")

        print(f"Probability maps saved to {prob_path}")

    # Print class distribution
    unique, counts_arr = np.unique(predictions[predictions != 255], return_counts=True)
    total_valid = counts_arr.sum()
    print("\nPredicted class distribution:")
    for val, count in zip(unique, counts_arr):
        if val < len(class_names):
            pct = count / total_valid * 100
            print(f"  {class_names[val]}: {count:,} pixels ({pct:.1f}%)")


def main(
    model_path: Path,
    input_path: Path,
    output_path: Path,
    stats_path: Path,
    patch_size: int = 128,
    overlap: int = 64,
    base_filters: int = 32,
    depth: int = 4,
    save_probabilities: bool = False,
    architecture: str = "unet",
):
    """
    Main prediction function.

    Args:
        model_path: Path to trained model
        input_path: Path to input raster
        output_path: Path for output classification
        stats_path: Path to normalization stats
        patch_size: Sliding window size
        overlap: Window overlap
        base_filters: Model base filters
        depth: Model depth
        save_probabilities: Save probability maps
    """
    device = get_device()
    print(f"Using device: {device}")

    # Load normalization stats
    stats = load_normalization_stats(stats_path)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])

    # Load model
    print(f"\nLoading model from {model_path}")
    model = load_model(model_path, device, in_channels, num_classes, base_filters, depth,
                       architecture=architecture)

    # Run prediction
    print(f"\nProcessing {input_path}")
    predict_raster(
        model=model,
        input_path=input_path,
        output_path=output_path,
        stats=stats,
        device=device,
        patch_size=patch_size,
        overlap=overlap,
        save_probabilities=save_probabilities
    )


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Apply wetland classification model to raster")
    parser.add_argument("input", type=Path, help="Input raster (predictor bands)")
    parser.add_argument("output", type=Path, help="Output classification raster")
    parser.add_argument("--model", type=Path, default=Path("Models/best_model.pth"))
    parser.add_argument("--stats", type=Path, default=Path("Data/Training_Data/normalization_stats.json"))
    parser.add_argument("--patch-size", type=int, default=128)
    parser.add_argument("--overlap", type=int, default=64)
    parser.add_argument("--base-filters", type=int, default=32)
    parser.add_argument("--depth", type=int, default=4)
    parser.add_argument("--probs", action="store_true", help="Save probability maps")
    parser.add_argument("--architecture", type=str, default="unet",
                        choices=["unet", "resunet34"],
                        help="Model architecture (default: unet)")

    args = parser.parse_args()

    # Handle relative paths
    project_root = Path(__file__).parent.parent.parent
    model_path = project_root / args.model if not args.model.is_absolute() else args.model
    stats_path = project_root / args.stats if not args.stats.is_absolute() else args.stats

    main(
        model_path=model_path,
        input_path=args.input,
        output_path=args.output,
        stats_path=stats_path,
        patch_size=args.patch_size,
        overlap=args.overlap,
        base_filters=args.base_filters,
        depth=args.depth,
        save_probabilities=args.probs,
        architecture=args.architecture,
    )
