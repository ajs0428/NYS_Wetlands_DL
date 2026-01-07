#!/usr/bin/env python
# coding: utf-8

# In[5]:


from pathlib import Path
import os
import json

workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_DL/")
os.chdir(workdir)
print(f"Current working directory: {Path.cwd()}")

# === CONFIGURATION ===
# Set these to match the output from NYS_03_create_patches_v2.ipynb
data_dir = Path("Data/Patches_v2")
cluster_id = 208  # Cluster to load, or None for legacy files
huc_id = None     # Specific HUC to load, or None to combine all HUCs in cluster

print(f"\nConfiguration:")
print(f"  data_dir: {data_dir}")
print(f"  cluster_id: {cluster_id}")
print(f"  huc_id: {huc_id or 'All HUCs in cluster'}")


# In[6]:


import torch
from torch.utils.data import Dataset, DataLoader
import numpy as np


# In[7]:


class WetlandDataset(Dataset):
    """PyTorch Dataset for wetland segmentation patches."""

    def __init__(self, X_path, y_path, metadata, normalize=True):
        """
        Args:
            X_path: Path to input patches numpy file (or list of paths)
            y_path: Path to label patches numpy file (or list of paths)
            metadata: Metadata dict containing band_names and normalization params
            normalize: Whether to normalize inputs
        """
        # Handle single file or list of files
        if isinstance(X_path, (list, tuple)):
            self.X = np.concatenate([np.load(p) for p in X_path], axis=0)
            self.y = np.concatenate([np.load(p) for p in y_path], axis=0)
        else:
            self.X = np.load(X_path)
            self.y = np.load(y_path)

        self.normalize = normalize
        self.metadata = metadata
        self.band_names = metadata["band_names"]
        self.normalization = metadata["normalization"]

    def __len__(self):
        return len(self.X)

    def __getitem__(self, idx):
        X = self.X[idx].astype(np.float32).copy()
        y = self.y[idx].astype(np.int64).copy()

        if self.normalize:
            for i, band_name in enumerate(self.band_names):
                norm_params = self.normalization[band_name]

                if norm_params["type"] == "divide":
                    X[i] = X[i] / norm_params["value"]
                elif norm_params["type"] == "shift_scale":
                    X[i] = (X[i] + norm_params["shift"]) / norm_params["scale"]
                elif norm_params["type"] == "minmax":
                    min_val = norm_params["min"]
                    max_val = norm_params["max"]
                    if max_val - min_val > 0:
                        X[i] = (X[i] - min_val) / (max_val - min_val)
                    else:
                        X[i] = 0.0  # Handle constant bands

        return torch.from_numpy(X), torch.from_numpy(y)


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
        pattern_base = f"cluster_{cluster_id}_*_{huc_id}_"
        return {
            "X_train": list(data_dir.glob(f"cluster_{cluster_id}_X_train_{huc_id}_.npy")),
            "y_train": list(data_dir.glob(f"cluster_{cluster_id}_y_train_{huc_id}_.npy")),
            "X_val": list(data_dir.glob(f"cluster_{cluster_id}_X_val_{huc_id}_.npy")),
            "y_val": list(data_dir.glob(f"cluster_{cluster_id}_y_val_{huc_id}_.npy")),
            "metadata": list(data_dir.glob(f"cluster_{cluster_id}_metadata_{huc_id}_.json"))[0],
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
        "metadata_files": metadata_files,  # Multiple metadata files
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
            "mean": sum(means) / len(means),  # Simple average
            "std": sum(stds) / len(stds),      # Approximate
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
    merged["hucs_included"] = [mf.stem.split("_")[-2] for mf in metadata_files]

    return merged


def get_dataloaders(data_dir, cluster_id=None, huc_id=None, batch_size=16):
    """
    Create training and validation DataLoaders.

    Args:
        data_dir: Directory containing patch files
        cluster_id: Cluster ID to load (None for legacy files)
        huc_id: Specific HUC to load (None for all HUCs in cluster)
        batch_size: Batch size for DataLoaders

    Returns:
        train_loader, val_loader, metadata
    """
    files = find_patch_files(data_dir, cluster_id, huc_id)

    # Load metadata
    if "metadata" in files:
        metadata = load_and_merge_metadata(files["metadata"])
    else:
        metadata = load_and_merge_metadata(files["metadata_files"])

    print(f"Found {len(files['X_train'])} training file(s)")
    print(f"Found {len(files['X_val'])} validation file(s)")

    train_dataset = WetlandDataset(
        files["X_train"],
        files["y_train"],
        metadata,
        normalize=True
    )
    val_dataset = WetlandDataset(
        files["X_val"],
        files["y_val"],
        metadata,
        normalize=True
    )

    train_loader = DataLoader(
        train_dataset,
        batch_size=batch_size,
        shuffle=True,
        num_workers=0
    )

    val_loader = DataLoader(
        val_dataset,
        batch_size=batch_size,
        shuffle=False,
        num_workers=0
    )

    return train_loader, val_loader, metadata


# In[ ]:




