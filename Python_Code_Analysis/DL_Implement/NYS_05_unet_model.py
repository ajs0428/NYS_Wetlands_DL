#!/usr/bin/env python
# coding: utf-8
"""
NYS_05_unet_model.py

U-Net model for wetland semantic segmentation.
Generated from NYS_05_unet_model.ipynb

Exports: UNet, ConvBlock, EncoderBlock, DecoderBlock,
         find_patch_files, load_and_merge_metadata
"""

from pathlib import Path
import os
import json

import torch
import torch.nn as nn


# === FILE LOADING UTILITIES ===

def find_patch_files(data_dir, cluster_id=None, huc_id=None):
    """
    Find patch files based on cluster/HUC configuration.

    Returns:
        dict with keys: X_train, y_train, X_val, y_val, metadata
        Each value is a list of file paths (or single path for metadata)
    """
    data_dir = Path(data_dir)

    if cluster_id is None:
        # Legacy mode: look for simple filenames
        return {
            "X_train": [data_dir / "X_train.npy"],
            "y_train": [data_dir / "y_train.npy"],
            "X_val": [data_dir / "X_val.npy"],
            "y_val": [data_dir / "y_val.npy"],
            "metadata": data_dir / "metadata.json",
        }

    if huc_id is not None:
        # Specific HUC
        return {
            "X_train": list(data_dir.glob(f"cluster_{cluster_id}_X_train_{huc_id}_.npy")),
            "y_train": list(data_dir.glob(f"cluster_{cluster_id}_y_train_{huc_id}_.npy")),
            "X_val": list(data_dir.glob(f"cluster_{cluster_id}_X_val_{huc_id}_.npy")),
            "y_val": list(data_dir.glob(f"cluster_{cluster_id}_y_val_{huc_id}_.npy")),
            "metadata": list(data_dir.glob(f"cluster_{cluster_id}_metadata_{huc_id}.json"))[0],
        }

    # All HUCs in cluster
    X_train_files = sorted(data_dir.glob(f"cluster_{cluster_id}_X_train_*.npy"))
    y_train_files = sorted(data_dir.glob(f"cluster_{cluster_id}_y_train_*.npy"))
    X_val_files = sorted(data_dir.glob(f"cluster_{cluster_id}_X_val_*.npy"))
    y_val_files = sorted(data_dir.glob(f"cluster_{cluster_id}_y_val_*.npy"))
    metadata_files = sorted(data_dir.glob(f"cluster_{cluster_id}_metadata_*.json"))

    if not X_train_files:
        raise FileNotFoundError(f"No training files found for cluster {cluster_id} in {data_dir}")

    return {
        "X_train": X_train_files,
        "y_train": y_train_files,
        "X_val": X_val_files,
        "y_val": y_val_files,
        "metadata_files": metadata_files,
    }


def load_and_merge_metadata(metadata_files):
    """
    Load and merge metadata from multiple HUC files.

    For band_stats, computes global min/max across all files.
    For normalization with minmax, updates to use global stats.
    """
    if isinstance(metadata_files, (str, Path)):
        # Single file
        with open(metadata_files) as f:
            return json.load(f)

    # Multiple files - merge them
    all_metadata = []
    for mf in metadata_files:
        with open(mf) as f:
            all_metadata.append(json.load(f))

    # Start with first file as base
    merged = all_metadata[0].copy()

    # Merge band_stats: compute global min/max
    band_names = merged["band_names"]
    merged_stats = {}

    for band in band_names:
        mins = [m["band_stats"][band]["min"] for m in all_metadata]
        maxs = [m["band_stats"][band]["max"] for m in all_metadata]
        means = [m["band_stats"][band]["mean"] for m in all_metadata]
        stds = [m["band_stats"][band]["std"] for m in all_metadata]

        merged_stats[band] = {
            "min": min(mins),
            "max": max(maxs),
            "mean": sum(means) / len(means),
            "std": sum(stds) / len(stds),
        }

    merged["band_stats"] = merged_stats

    # Update minmax normalization to use global stats
    for band, norm in merged["normalization"].items():
        if norm["type"] == "minmax":
            norm["min"] = merged_stats[band]["min"]
            norm["max"] = merged_stats[band]["max"]

    # Sum up counts
    merged["n_train"] = sum(m["n_train"] for m in all_metadata)
    merged["n_val"] = sum(m["n_val"] for m in all_metadata)
    merged["hucs_included"] = [mf.stem.split("_")[-1] for mf in metadata_files]

    return merged


# === U-NET MODEL COMPONENTS ===

class ConvBlock(nn.Module):
    """Two consecutive conv layers with BatchNorm and ReLU."""

    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.conv = nn.Sequential(
            nn.Conv2d(in_channels, out_channels, kernel_size=3, padding=1),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True),
            nn.Conv2d(out_channels, out_channels, kernel_size=3, padding=1),
            nn.BatchNorm2d(out_channels),
            nn.ReLU(inplace=True)
        )

    def forward(self, x):
        return self.conv(x)


class EncoderBlock(nn.Module):
    """ConvBlock followed by MaxPool for downsampling."""

    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.conv = ConvBlock(in_channels, out_channels)
        self.pool = nn.MaxPool2d(kernel_size=2, stride=2)

    def forward(self, x):
        conv_out = self.conv(x)
        pooled = self.pool(conv_out)
        return conv_out, pooled  # Return both for skip connection


class DecoderBlock(nn.Module):
    """Upsample, concatenate skip connection, then ConvBlock."""

    def __init__(self, in_channels, out_channels):
        super().__init__()
        self.upsample = nn.ConvTranspose2d(
            in_channels, out_channels, kernel_size=2, stride=2
        )
        self.conv = ConvBlock(out_channels * 2, out_channels)  # *2 for concatenation

    def forward(self, x, skip):
        x = self.upsample(x)
        x = torch.cat([x, skip], dim=1)  # Concatenate along channel dimension
        return self.conv(x)


class UNet(nn.Module):
    """Lightweight U-Net for semantic segmentation."""

    def __init__(self, in_channels, num_classes, base_filters=32):
        """
        Args:
            in_channels: Number of input bands (from metadata)
            num_classes: Number of output classes (from metadata)
            base_filters: Number of filters in first layer (doubles each level)
        """
        super().__init__()

        f = base_filters  # 32

        # Encoder path
        self.enc1 = EncoderBlock(in_channels, f)
        self.enc2 = EncoderBlock(f, f * 2)
        self.enc3 = EncoderBlock(f * 2, f * 4)
        self.enc4 = EncoderBlock(f * 4, f * 8)

        # Bottleneck
        self.bottleneck = ConvBlock(f * 8, f * 16)

        # Decoder path
        self.dec4 = DecoderBlock(f * 16, f * 8)
        self.dec3 = DecoderBlock(f * 8, f * 4)
        self.dec2 = DecoderBlock(f * 4, f * 2)
        self.dec1 = DecoderBlock(f * 2, f)

        # Final classification layer
        self.final = nn.Conv2d(f, num_classes, kernel_size=1)

    def forward(self, x):
        # Encoder
        skip1, x = self.enc1(x)
        skip2, x = self.enc2(x)
        skip3, x = self.enc3(x)
        skip4, x = self.enc4(x)

        # Bottleneck
        x = self.bottleneck(x)

        # Decoder with skip connections
        x = self.dec4(x, skip4)
        x = self.dec3(x, skip3)
        x = self.dec2(x, skip2)
        x = self.dec1(x, skip1)

        # Output
        return self.final(x)


# === MAIN (only runs when script is executed directly) ===
if __name__ == "__main__":
    import numpy as np

    # Configuration
    workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_GHG/")
    os.chdir(workdir)
    print(f"Current working directory: {Path.cwd()}")

    data_dir = Path("Data/Patches_v2")
    cluster_id = 208
    huc_id = None

    # Load metadata
    files = find_patch_files(data_dir, cluster_id, huc_id)
    if "metadata" in files:
        metadata = load_and_merge_metadata(files["metadata"])
    else:
        metadata = load_and_merge_metadata(files["metadata_files"])

    print(f"\nMetadata loaded:")
    print(f"  in_channels: {metadata['in_channels']}")
    print(f"  num_classes: {metadata['num_classes']}")
    print(f"  patch_size: {metadata['patch_size']}")

    # Test model
    model = UNet(
        in_channels=metadata["in_channels"],
        num_classes=metadata["num_classes"],
        base_filters=32
    )

    total_params = sum(p.numel() for p in model.parameters())
    print(f"\nTotal parameters: {total_params:,}")

    # Test forward pass
    dummy_input = torch.randn(4, metadata["in_channels"], metadata["patch_size"], metadata["patch_size"])
    output = model(dummy_input)
    print(f"Input shape: {dummy_input.shape}")
    print(f"Output shape: {output.shape}")
    print("\nModel architecture verified successfully!")
