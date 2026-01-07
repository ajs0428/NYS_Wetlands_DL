#!/usr/bin/env python
# coding: utf-8

# # NYS_03_create_patches_v2

# In[1]:


from pathlib import Path
import os
workdir = Path("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_DL/")
print(workdir)
os.chdir(workdir)
current_working_dir = Path.cwd()
print(f"Current working directory is now: {current_working_dir}")


# In[2]:


import rasterio
import geopandas as gpd
import numpy as np
from pathlib import Path
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import json


# In[3]:


# === Testing Args ===
args = ["Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
        208,
        "%",
        "Data/TerrainProcessed/HUC_DEMs/",
        "Data/Training_Data/HUC_Extracted_Training_Data/",
        "Data/NAIP/HUC_NAIP_Processed/",
        "Data/CHMs/HUC_CHMs/",
        "Data/TerrainProcessed/HUC_TerrainMetrics/"]


# In[4]:


# === Cluster Import ===
ny_hucs = gpd.read_file(args[0], where=f"cluster = '{args[1]}' AND huc12 LIKE '{args[2]}'")
ny_hucs


# In[5]:


# === CONFIGURATION ===

patch_size = 256
max_offset = 32  # Random offset from centroid (pixels) to add variety
background_patches = 120  # Number of random background patches to include
val_split = 0.2
random_seed = 42

output_dir = Path("Data/Patches_v2")


# In[6]:


# === RASTER INPUT CONFIGURATION ===
# Define input rasters and how to handle their bands
# - "path_pattern": glob pattern with {huc} placeholder
# - "bands": list of band names, or None to read from raster descriptions
raster_inputs = [
    {
        "name": "naip",
        "path_pattern": "Data/NAIP/HUC_NAIP_Processed/*{huc}*.tif",
        "bands": None,  # Read from raster descriptions (r, g, b, nir, ndvi, ndwi)
    },
    {
        "name": "dem",
        "path_pattern": "Data/TerrainProcessed/HUC_DEMs/*{huc}.tif",
        "bands": ["dem"],  # Single band, manually named
    },
    {
        "name": "chm",
        "path_pattern": "Data/CHMs/HUC_CHMs/*{huc}*.tif",
        "bands": ["chm"],  # Single band, manually named
    },
    {
        "name": "terrain",
        "path_pattern": "Data/TerrainProcessed/HUC_TerrainMetrics/*{huc}*5m.tif",
        "bands": None,  # Read from descriptions (slope_5m, TPI_5m, Geomorph_5m)
    },
]

# === NORMALIZATION RULES ===
# Define normalization strategy for known band names
# Bands not listed here will default to "minmax" using computed stats
normalization_rules = {
    # NAIP spectral bands (0-255 range)
    "r": {"type": "divide", "value": 255.0},
    "g": {"type": "divide", "value": 255.0},
    "b": {"type": "divide", "value": 255.0},
    "nir": {"type": "divide", "value": 255.0},
    # Spectral indices (-1 to 1 range)
    "ndvi": {"type": "shift_scale", "shift": 1.0, "scale": 2.0},
    "ndwi": {"type": "shift_scale", "shift": 1.0, "scale": 2.0},
    # Categorical variables (fixed scaling)
    "Geomorph_5m": {"type": "divide", "value": 10.0},
}

print(f"Configured {len(raster_inputs)} raster inputs:")
for r in raster_inputs:
    print(f"  - {r['name']}: {r['path_pattern']}")
print(f"\nNormalization rules defined for {len(normalization_rules)} bands")
print(f"Unknown bands will use minmax normalization")


# In[7]:


# === HELPER FUNCTION: EXTRACT PATCH ===
def extract_patch(center_row, center_col, patch_size, inputs, labels):
    """
    Extract a patch centered at (center_row, center_col).
    Returns None if patch would be out of bounds or contains NaN.
    """
    half = patch_size // 2

    # Calculate bounds
    row_start = center_row - half
    row_end = center_row + half
    col_start = center_col - half
    col_end = center_col + half

    # Check bounds
    if row_start < 0 or row_end > height or col_start < 0 or col_end > width:
        return None, None

    # Extract patches
    X_patch = inputs[:, row_start:row_end, col_start:col_end]
    y_patch = labels[row_start:row_end, col_start:col_end]

    # Check for NaN
    if np.any(np.isnan(X_patch)):
        return None, None

    return X_patch, y_patch


# In[8]:


for i in ny_hucs['huc12'][0:1]:
    print(f"\n{'='*60}")
    print(f"Processing HUC: {i}")
    print(f"{'='*60}")

    # === LOAD BANDS DYNAMICALLY FROM CONFIGURATION ===
    bands = {}
    band_names = []
    transform = None

    for raster_cfg in raster_inputs:
        # Build glob pattern with HUC substitution
        pattern = raster_cfg["path_pattern"].replace("{huc}", i)
        matches = list(Path(".").glob(pattern))

        if not matches:
            raise FileNotFoundError(f"No files found for {raster_cfg['name']}: {pattern}")

        raster_path = matches[0]
        print(f"  Loading {raster_cfg['name']}: {raster_path.name}")

        with rasterio.open(raster_path) as src:
            data = src.read()

            # Keep transform from first raster (for coordinate conversion)
            if transform is None:
                transform = src.transform

            # Determine band names
            if raster_cfg["bands"] is not None:
                # Use manually specified band names
                names = raster_cfg["bands"]
            elif src.descriptions and all(src.descriptions):
                # Read from raster descriptions
                names = list(src.descriptions)
            else:
                # Fallback: generate names like "raster_name_1", "raster_name_2"
                names = [f"{raster_cfg['name']}_{j+1}" for j in range(src.count)]

            # Validate band count matches
            if len(names) != data.shape[0]:
                raise ValueError(
                    f"Band count mismatch for {raster_cfg['name']}: "
                    f"got {len(names)} names but {data.shape[0]} bands"
                )

            # Store each band individually
            for idx, name in enumerate(names):
                if name in bands:
                    raise ValueError(f"Duplicate band name: {name}")
                bands[name] = data[idx:idx+1]  # Keep as (1, H, W)
                band_names.append(name)

    # === LOAD LABELS AND WETLANDS ===
    labels_path = f"Data/Training_Data/cluster_{args[1]}_huc_{i}_labels.tif"
    wetlands_pattern = f"Data/Training_Data/HUC_Extracted_Training_Data/*{i}*.gpkg"
    wetlands_path = list(Path(".").glob(wetlands_pattern))[0]

    with rasterio.open(labels_path) as src:
        labels = src.read(1)

    wetlands = gpd.read_file(wetlands_path)

    # === STACK BANDS IN ORDER LOADED ===
    inputs = np.vstack([bands[name] for name in band_names])
    _, height, width = inputs.shape

    print(f"\nInput stack shape: {inputs.shape}")
    print(f"Labels shape: {labels.shape}")
    print(f"Band names ({len(band_names)}): {band_names}")

    # === EXTRACT WETLAND-CENTERED PATCHES ===
    wetland_patches_X = []
    wetland_patches_y = []
    skipped_count = 0

    for idx, row in wetlands.iterrows():
        # Get centroid coordinates
        centroid = row.geometry.centroid

        # Convert geographic coordinates to pixel coordinates
        col, row_px = ~transform * (centroid.x, centroid.y)
        col, row_px = int(col), int(row_px)

        # Add random offset for variety
        offset_row = np.random.randint(-max_offset, max_offset + 1)
        offset_col = np.random.randint(-max_offset, max_offset + 1)
        center_row = row_px + offset_row
        center_col = col + offset_col

        # Extract patch
        X_patch, y_patch = extract_patch(center_row, center_col, patch_size, inputs, labels)

        if X_patch is not None:
            wetland_patches_X.append(X_patch)
            wetland_patches_y.append(y_patch)
        else:
            skipped_count += 1
    print(f"Wetland-centered patches extracted: {len(wetland_patches_X)}")
    print(f"Skipped (out of bounds or NaN): {skipped_count}")

    # === EXTRACT RANDOM BACKGROUND PATCHES ===
    background_patches_X = []
    background_patches_y = []
    attempts = 0
    max_attempts = background_patches * 10

    while len(background_patches_X) < background_patches and attempts < max_attempts:
        attempts += 1

        center_row = np.random.randint(patch_size // 2, height - patch_size // 2)
        center_col = np.random.randint(patch_size // 2, width - patch_size // 2)

        X_patch, y_patch = extract_patch(center_row, center_col, patch_size, inputs, labels)

        if X_patch is None:
            continue

        if not np.any(y_patch > 0):
            background_patches_X.append(X_patch)
            background_patches_y.append(y_patch)

    print(f"Background patches extracted: {len(background_patches_X)}")

    # === COMBINE AND SPLIT ===
    all_X = wetland_patches_X + background_patches_X
    all_y = wetland_patches_y + background_patches_y

    X_array = np.array(all_X, dtype=np.float32)
    y_array = np.array(all_y, dtype=np.uint8)

    print(f"Total patches: {len(X_array)}")
    print(f"X shape: {X_array.shape}")
    print(f"y shape: {y_array.shape}")

    # Train/val split
    X_train, X_val, y_train, y_val = train_test_split(
        X_array, y_array, 
        test_size=val_split, 
        random_state=random_seed
    )

    print(f"\nTrain patches: {len(X_train)}")
    print(f"Validation patches: {len(X_val)}")

    # === SAVE PATCHES ===
    np.save(output_dir / f"cluster_{args[1]}_X_train_{i}_.npy", X_train)
    np.save(output_dir / f"cluster_{args[1]}_y_train_{i}_.npy", y_train)
    np.save(output_dir / f"cluster_{args[1]}_X_val_{i}_.npy", X_val)
    np.save(output_dir / f"cluster_{args[1]}_y_val_{i}_.npy", y_val)

    # === COMPUTE BAND STATISTICS FROM TRAINING DATA ===
    print("Computing band statistics from training data...")
    band_stats = {}
    for j, name in enumerate(band_names):
        band_data = X_train[:, j, :, :]
        band_stats[name] = {
            "min": float(np.nanmin(band_data)),
            "max": float(np.nanmax(band_data)),
            "mean": float(np.nanmean(band_data)),
            "std": float(np.nanstd(band_data)),
        }
        print(f"  {name}: min={band_stats[name]['min']:.3f}, max={band_stats[name]['max']:.3f}")

    # === BUILD NORMALIZATION FROM RULES (with minmax fallback) ===
    normalization = {}
    for name in band_names:
        if name in normalization_rules:
            # Use predefined rule
            normalization[name] = normalization_rules[name].copy()
        else:
            # Default to minmax normalization using computed stats
            normalization[name] = {
                "type": "minmax",
                "min": band_stats[name]["min"],
                "max": band_stats[name]["max"]
            }
            print(f"  Note: '{name}' not in normalization_rules, using minmax")

    # === SAVE METADATA ===
    metadata = {
        "in_channels": int(X_train.shape[1]),
        "num_classes": 5,
        "patch_size": patch_size,
        "band_names": band_names,
        "class_names": ["Background", "EMW", "FSW", "SSW", "OWW"],
        "n_train": int(len(X_train)),
        "n_val": int(len(X_val)),
        "band_stats": band_stats,
        "normalization": normalization,
        "raster_inputs": raster_inputs,  # Save config for reproducibility
    }

    with open(output_dir / f"cluster_{args[1]}_metadata_{i}_.json", "w") as f:
        json.dump(metadata, f, indent=2)

    print(f"\nSaved patches to {output_dir}")
    print(f"Saved metadata with band statistics and normalization parameters")






