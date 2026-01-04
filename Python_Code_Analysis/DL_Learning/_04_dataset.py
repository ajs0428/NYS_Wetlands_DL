from pathlib import Path
import os
import json

workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_GHG/")
os.chdir(workdir)

import torch
from torch.utils.data import Dataset, DataLoader
import numpy as np


def load_metadata(data_dir="Data/Patches_v2"):
    """Load metadata from patches directory."""
    metadata_path = Path(data_dir) / "metadata.json"
    if metadata_path.exists():
        with open(metadata_path) as f:
            return json.load(f)
    else:
        raise FileNotFoundError(f"Metadata not found at {metadata_path}. Run 03_create_patches_v2.ipynb first.")


class WetlandDataset(Dataset):
    """PyTorch Dataset for wetland segmentation patches."""

    def __init__(self, X_path, y_path, metadata, normalize=True):
        """
        Args:
            X_path: Path to input patches numpy file
            y_path: Path to label patches numpy file
            metadata: Metadata dict containing band_names and normalization params
            normalize: Whether to normalize inputs
        """
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
                    X[i] = (X[i] - min_val) / (max_val - min_val)

        return torch.from_numpy(X), torch.from_numpy(y)


def get_dataloaders(data_dir="Data/Patches_v2", batch_size=16):
    """Create training and validation DataLoaders using metadata."""
    data_dir = Path(data_dir)
    metadata = load_metadata(data_dir)

    train_dataset = WetlandDataset(
        data_dir / "X_train.npy",
        data_dir / "y_train.npy",
        metadata,
        normalize=True
    )
    val_dataset = WetlandDataset(
        data_dir / "X_val.npy",
        data_dir / "y_val.npy",
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


# === TEST THE DATASET ===
if __name__ == "__main__":
    data_dir = "Data/Patches_v2"
    train_loader, val_loader, metadata = get_dataloaders(data_dir, batch_size=16)

    print(f"Loaded metadata from {data_dir}")
    print(f"  Bands: {metadata['band_names']}")
    print(f"  Patch size: {metadata['patch_size']}")

    print(f"\nTraining batches: {len(train_loader)}")
    print(f"Validation batches: {len(val_loader)}")

    # Get one batch and check shapes/ranges
    X_batch, y_batch = next(iter(train_loader))

    print(f"\nBatch X shape: {X_batch.shape}")
    print(f"Batch y shape: {y_batch.shape}")
    print(f"X dtype: {X_batch.dtype}")
    print(f"y dtype: {y_batch.dtype}")

    print("\nNormalized band ranges:")
    for i, name in enumerate(metadata["band_names"]):
        band = X_batch[:, i, :, :]
        print(f"  {name}: min={band.min():.3f}, max={band.max():.3f}")

    print(f"\nLabel classes in batch: {torch.unique(y_batch).tolist()}")