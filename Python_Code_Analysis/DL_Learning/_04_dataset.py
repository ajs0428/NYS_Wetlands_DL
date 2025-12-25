from pathlib import Path
import os
workdir = Path("/ibstorage/anthony/NYS_Wetlands_GHG/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")

import torch
from torch.utils.data import Dataset, DataLoader
import numpy as np

class WetlandDataset(Dataset):
    """PyTorch Dataset for wetland segmentation patches."""
    
    def __init__(self, X_path, y_path, normalize=True):
        """
        Args:
            X_path: Path to input patches numpy file (N, 7, 128, 128)
            y_path: Path to label patches numpy file (N, 128, 128)
            normalize: Whether to normalize inputs
        """
        self.X = np.load(X_path)
        self.y = np.load(y_path)
        self.normalize = normalize
        
        # Normalization parameters
        # Bands: R, G, B, NIR, NDWI, NDVI, DEM
        self.rgb_nir_max = 255.0
        self.ndwi_ndvi_range = (-1.0, 1.0)
        self.dem_range = (311.0, 510.0)  # From observed data
        
    def __len__(self):
        return len(self.X)
    
    def __getitem__(self, idx):
        X = self.X[idx].astype(np.float32).copy()
        y = self.y[idx].astype(np.int64).copy()
        
        if self.normalize:
            # R, G, B, NIR (bands 0-3): divide by 255
            X[0:4] = X[0:4] / self.rgb_nir_max
            
            # NDWI, NDVI (bands 4-5): shift from [-1,1] to [0,1]
            X[4:6] = (X[4:6] + 1.0) / 2.0
            
            # DEM (band 6): min-max normalize
            X[6] = (X[6] - self.dem_range[0]) / (self.dem_range[1] - self.dem_range[0])
        
        return torch.from_numpy(X), torch.from_numpy(y)


def get_dataloaders(train_X_path, train_y_path, val_X_path, val_y_path, batch_size=16):
    """Create training and validation DataLoaders."""
    
    train_dataset = WetlandDataset(train_X_path, train_y_path, normalize=True)
    val_dataset = WetlandDataset(val_X_path, val_y_path, normalize=True)
    
    train_loader = DataLoader(
        train_dataset,
        batch_size=batch_size,
        shuffle=True,
        num_workers=0  # Set to 0 for Windows compatibility; increase on Linux
    )
    
    val_loader = DataLoader(
        val_dataset,
        batch_size=batch_size,
        shuffle=False,
        num_workers=0
    )
    
    return train_loader, val_loader


# === TEST THE DATASET ===
if __name__ == "__main__":
    train_loader, val_loader = get_dataloaders(
        "Data/Patches/X_train.npy",
        "Data/Patches/y_train.npy",
        "Data/Patches/X_val.npy",
        "Data/Patches/y_val.npy",
        batch_size=16
    )
    
    print(f"Training batches: {len(train_loader)}")
    print(f"Validation batches: {len(val_loader)}")
    
    # Get one batch and check shapes/ranges
    X_batch, y_batch = next(iter(train_loader))
    
    print(f"\nBatch X shape: {X_batch.shape}")  # Should be (16, 7, 128, 128)
    print(f"Batch y shape: {y_batch.shape}")    # Should be (16, 128, 128)
    print(f"X dtype: {X_batch.dtype}")
    print(f"y dtype: {y_batch.dtype}")
    
    print("\nNormalized band ranges:")
    band_names = ['R', 'G', 'B', 'NIR', 'NDWI', 'NDVI', 'DEM']
    for i, name in enumerate(band_names):
        band = X_batch[:, i, :, :]
        print(f"  {name}: min={band.min():.3f}, max={band.max():.3f}")
    
    print(f"\nLabel classes in batch: {torch.unique(y_batch).tolist()}")