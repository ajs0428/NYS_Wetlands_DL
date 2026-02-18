# NYS Wetlands DL Pipeline v2 — User Guide

Deep learning pipeline for wetland semantic segmentation in New York State using a U-Net architecture. Supports two classification modes: **multiclass** (5-class: EMW, FSW, OWW, SSW, UPL) and **binary** (WET vs UPL). The mode is controlled by a single toggle in `band_config.json` — both modes use the same training patches with label remapping applied at runtime.

## Table of Contents

- [Quick Start](#quick-start)
- [Environment Setup](#environment-setup)
- [Glossary of Acronyms](#glossary-of-acronyms)
- [Pipeline Overview](#pipeline-overview)
- [Input Data Requirements](#input-data-requirements)
- [Configuration: band_config.json](#configuration-band_configjson)
- [Classification Mode: Multiclass vs Binary](#classification-mode-multiclass-vs-binary)
- [Shared Utilities: band_utils.py](#shared-utilities-band_utilspy)
- [Step 1: Compute Statistics](#step-1-compute-statistics-01_compute_statisticspy)
- [Step 2: Dataset & DataLoaders](#step-2-dataset--dataloaders-02_datasetpy)
- [Step 3: Model Architecture](#step-3-model-architecture-03_unet_modelpy)
- [Step 4: Train](#step-4-train-04_trainpy)
- [Step 5: Evaluate](#step-5-evaluate-05_evaluatepy)
- [Step 6: Predict](#step-6-predict-06_predictpy)
- [Interactive Notebook](#interactive-notebook-wetland_pipelineipynb)
- [Adding a New Band](#adding-a-new-band)
- [Patch Size](#patch-size)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
# 1. Activate the environment
conda activate wetland-cnn

# 2. Navigate to the pipeline directory
cd "Python_Code_Analysis/DL_Pipeline_v2"

# 3. Compute normalization statistics from training patches
python 01_compute_statistics.py \
  --patches-dir "../../Data/Training_Data/R_Patches"

# 4. Train the model
python 04_train.py --epochs 50 --batch-size 16

# 5. Evaluate on the held-out test set
python 05_evaluate.py --model "../../Models/best_model.pth"

# 6. Run inference on a new raster
python 06_predict.py input_raster.tif output_classification.tif --probs
```

---

## Environment Setup

```bash
conda env create -f Python_Code_Analysis/wetland-cnn-env.yml
conda activate wetland-cnn
```

Key dependencies: PyTorch, rasterio, NumPy, scikit-learn, matplotlib.

---

## Glossary of Acronyms

### Wetland Classes

| Acronym | Full Name | Description |
|---------|-----------|-------------|
| EMW | Emergent Wetland | Wetlands dominated by herbaceous vegetation (grasses, sedges) |
| FSW | Forested Wetland | Wetlands dominated by trees (>6 m tall) |
| OWW | Open Water Wetland | Wetlands with standing or flowing open water |
| SSW | Scrub-Shrub Wetland | Wetlands dominated by woody shrubs (<6 m tall) |
| UPL | Upland | Confirmed non-wetland land (Background class) |
| NWI | National Wetlands Inventory | USFWS dataset used as label source |

### Remote Sensing Bands — Terrain

| Acronym | Full Name | Description |
|---------|-----------|-------------|
| DEM | Digital Elevation Model | Ground surface elevation (meters) |
| CHM | Canopy Height Model | Vegetation height above ground (meters), derived from LiDAR |
| TPI | Topographic Position Index | Relative elevation compared to surrounding area; positive = ridge, negative = valley |
| meanc | Mean Curvature | Average surface curvature (concavity/convexity) |
| planc | Plan Curvature | Curvature perpendicular to slope direction; indicates flow convergence/divergence |
| profc | Profile Curvature | Curvature in the direction of slope; indicates acceleration/deceleration of flow |
| dmv | Deviation from Mean Value | Local deviation of elevation from the neighborhood mean |
| Geomorph | Geomorphon | Landform classification (10 categories: flat, peak, ridge, shoulder, spur, slope, hollow, footslope, valley, pit) |

### Remote Sensing Bands — Spectral Indices (Optical)

| Acronym | Full Name | Formula Concept | What It Measures |
|---------|-----------|-----------------|------------------|
| NDVI | Normalized Difference Vegetation Index | (NIR - Red) / (NIR + Red) | Live green vegetation vigor |
| MNDWI | Modified Normalized Difference Water Index | (Green - SWIR) / (Green + SWIR) | Surface water presence |
| EVI | Enhanced Vegetation Index | Adjusted NIR/Red ratio | Vegetation with atmospheric correction |
| NDYI | Normalized Difference Yellowness Index | (Green - Blue) / (Green + Blue) | Vegetation senescence / yellow coloring |
| PSRI | Plant Senescence Reflectance Index | (Red - Green) / NIR | Leaf aging and carotenoid pigments |
| GDVI | Green Difference Vegetation Index | NIR - Green | Green vegetation density |

### Remote Sensing Bands — SAR (Radar)

| Acronym | Full Name | Description |
|---------|-----------|-------------|
| SAR | Synthetic Aperture Radar | Active microwave sensor; penetrates clouds and captures surface structure |
| VV | Vertical-Vertical Polarization | SAR backscatter with vertical transmit and vertical receive |
| VH | Vertical-Horizontal Polarization | SAR backscatter with vertical transmit and horizontal receive; sensitive to vegetation volume |
| DPSVI | Dual-Pol SAR Vegetation Index | Vegetation index derived from VV and VH polarizations |
| RVI | Radar Vegetation Index | Ratio-based vegetation measure from SAR polarizations |
| VH/VV ratio | Cross-pol Ratio | VH divided by VV; indicates depolarization from vegetation scattering |

### Machine Learning Terms

| Acronym | Full Name | Description |
|---------|-----------|-------------|
| IoU | Intersection over Union | Overlap between predicted and true regions; primary segmentation metric |
| F1 | F1 Score | Harmonic mean of precision and recall |
| mIoU | Mean IoU | Average IoU across all classes |
| CRS | Coordinate Reference System | Spatial reference (e.g., EPSG:26918 for NYS) |
| MPS | Metal Performance Shaders | Apple Silicon GPU backend for PyTorch |
| HPC | High Performance Computing | Remote cluster with NVIDIA GPUs |
| HUC | Hydrologic Unit Code | USGS watershed boundary identifier (HUC12 = 12-digit) |

---

## Pipeline Overview

```
GeoTIFF Patches (21 bands: 20 predictors + 1 label)
        │
        ▼
 ┌──────────────────┐
 │ 01_compute_stats  │ → normalization_stats.json
 └──────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ 02_dataset        │ → PyTorch DataLoaders (train / val / test)
 └──────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ 03_unet_model     │ → U-Net architecture
 └──────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ 04_train          │ → best_model.pth, final_model.pth, training_history.json
 └──────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ 05_evaluate       │ → Per-class metrics, confusion matrix
 └──────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ 06_predict        │ → Classification GeoTIFF + probability maps
 └──────────────────┘
```

---

## Input Data Requirements

Training patches are GeoTIFF files located in `Data/Training_Data/R_Patches/`. The current patches are 128x128 pixels, but the pipeline supports any square patch size (see [Patch Size](#patch-size) below).

Each patch contains 21 bands (20 predictors + 1 label). Band names are stored in the GeoTIFF band descriptions and are discovered at runtime — no hardcoded indices.

**Current band layout:**

| Index | Band Name | Category |
|-------|-----------|----------|
| 0 | DEM | Terrain |
| 1 | meanc_local | Terrain (curvature) |
| 2 | planc_local | Terrain (curvature) |
| 3 | profc_local | Terrain (curvature) |
| 4 | dmv_local | Terrain |
| 5 | slope_local | Terrain |
| 6 | TPI_local | Terrain |
| 7 | Geomorph_local | Categorical (10 classes) |
| 8 | CHM | Terrain |
| 9 | NDVI | Spectral index |
| 10 | MNDWI | Spectral index |
| 11 | EVI | Spectral index |
| 12 | NDYI | Spectral index |
| 13 | PSRI | Spectral index |
| 14 | GDVI | Spectral index |
| 15 | VV | SAR backscatter |
| 16 | VH | SAR backscatter |
| 17 | DPSVI | SAR index |
| 18 | RVI | SAR index |
| 19 | VH_VV_ratio | SAR index |
| 20 | MOD_CLASS | Label |

**Classes (multiclass mode):**

| Value | Code | Description |
|-------|------|-------------|
| 0 | EMW | Emergent Wetland |
| 1 | FSW | Forested Wetland |
| 2 | OWW | Open Water Wetland |
| 3 | SSW | Scrub-Shrub Wetland |
| 4 | UPL | Upland / Background |
| 255 | — | Unlabeled (excluded from training) |

**Classes (binary mode):** EMW/FSW/OWW/SSW are remapped to WET (0), UPL stays as UPL (1). See [Classification Mode](#classification-mode-multiclass-vs-binary).

---

## Configuration: band_config.json

The single file to edit when changing band normalization. Located alongside the scripts.

```json
{
  "label_band": "MOD_CLASS",
  "classification_mode": "multiclass",
  "binary_mapping": {
    "WET": ["EMW", "FSW", "OWW", "SSW"],
    "UPL": ["UPL"]
  },
  "default_method": "min_max",
  "band_normalization": {
    "NDVI":  {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "MNDWI": {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "NDYI":  {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "Geomorph_local": {"method": "one_hot", "num_classes": 10, "class_range": [1, 10]}
  },
  "class_names": ["EMW", "FSW", "OWW", "SSW", "UPL"],
  "ignore_index": 255
}
```

**Normalization methods:**

| Method | Formula | Use Case |
|--------|---------|----------|
| `min_max` | (x - min) / (max - min) → [0, 1] | Default for continuous bands (DEM, CHM, slope, SAR, etc.) |
| `shift_scale` | (x + shift) / scale → [0, 1] | Spectral indices with known range (e.g., NDVI: [-1, 1]) |
| `one_hot` | Encode to N binary channels | Categorical bands (Geomorph) |

Any band **not listed** in `band_normalization` automatically uses `min_max`. You only need to add entries for `shift_scale` or `one_hot` bands.

---

## Classification Mode: Multiclass vs Binary

The pipeline supports two classification modes, controlled by `classification_mode` in `band_config.json`:

| Mode | Classes | Output Bands | Use Case |
|------|---------|--------------|----------|
| `"multiclass"` | EMW, FSW, OWW, SSW, UPL (5) | 5 | Fine-grained wetland type mapping |
| `"binary"` | WET, UPL (2) | 2 | Wetland presence/absence detection |

### Switching Modes

1. Edit `classification_mode` in `band_config.json` to `"binary"` or `"multiclass"`
2. Re-run `01_compute_statistics.py` to regenerate `normalization_stats.json`
3. Train, evaluate, and predict as usual — all downstream scripts adapt automatically

### How It Works

- The `binary_mapping` field in `band_config.json` defines how original classes group into binary classes. The key order determines integer encoding (WET=0, UPL=1).
- `01_compute_statistics.py` builds a `label_remap` dict (e.g., `{0:0, 1:0, 2:0, 3:0, 4:1}`) and aggregates class counts under the binary labels. Both `label_remap` and `classification_mode` are stored in `normalization_stats.json`.
- `02_dataset.py` reads `label_remap` from the stats and applies it on-the-fly via a vectorized numpy lookup table. The original training patches are never modified.
- Downstream scripts (`04_train`, `05_evaluate`, `06_predict`) derive `num_classes` from `len(stats["class_names"])`, so they work with either 2 or 5 classes without any code changes.

### Custom Groupings

You can define any label grouping by editing `binary_mapping`. For example, to separate forested wetlands from other wetland types:

```json
"classification_mode": "binary",
"binary_mapping": {
  "FSW": ["FSW"],
  "OTHER": ["EMW", "OWW", "SSW", "UPL"]
}
```

---

## Shared Utilities: band_utils.py

Imported by all pipeline scripts. Key functions:

| Function | Purpose |
|----------|---------|
| `load_band_config(path)` | Load `band_config.json` |
| `discover_bands_from_raster(path)` | Read band names from GeoTIFF descriptions |
| `get_predictor_band_names(names, label)` | Return band names excluding the label band |
| `get_normalization_method(band, config)` | Look up a band's normalization (with default fallback) |
| `compute_in_channels(names, config)` | Count total input channels after one-hot expansion |
| `compute_in_channels_from_stats(path)` | Read `in_channels` from `normalization_stats.json` |
| `validate_prediction_bands(raster, expected, label)` | Match raster bands to expected predictors by name |

---

## Step 1: Compute Statistics (`01_compute_statistics.py`)

Scans all training patches to compute per-band normalization statistics and class frequencies. Produces `normalization_stats.json`, which is the single source of truth for all downstream scripts.

### Usage

```bash
python 01_compute_statistics.py \
  --patches-dir ../../Data/Training_Data/R_Patches \
  --output ../../Data/Training_Data/normalization_stats.json \
  --config band_config.json
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--patches-dir` | `Data/Training_Data/R_Patches` | Directory containing GeoTIFF training patches |
| `--output` | `Data/Training_Data/normalization_stats.json` | Output path for the stats JSON |
| `--config` | Auto-detected | Path to `band_config.json` |

### What It Computes

- Per-band min, max, mean, and standard deviation
- Per-class pixel counts and inverse frequency class weights
- `in_channels` (total model input channels, including one-hot expansion)
- `predictor_names` and `label_band` for downstream scripts
- In binary mode: `label_remap` mapping and aggregated binary class counts

### Output

`normalization_stats.json` — example structure:

```json
{
  "num_patches": 1234,
  "in_channels": 29,
  "label_band": "MOD_CLASS",
  "predictor_names": ["DEM", "meanc_local", "..."],
  "normalization": {
    "DEM": {"method": "min_max", "min": 50.0, "max": 610.0},
    "NDVI": {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "Geomorph_local": {"method": "one_hot", "num_classes": 10}
  },
  "class_counts": {"EMW": 500000, "FSW": 250000, "...": "..."},
  "class_weights": {"EMW": 1.0, "FSW": 1.96, "...": "..."}
}
```

---

## Step 2: Dataset & DataLoaders (`02_dataset.py`)

PyTorch `Dataset` class for lazy-loading GeoTIFF patches with on-the-fly normalization.

### How It Works

1. Reads patches on demand via rasterio (not loaded into memory all at once)
2. Separates predictor bands from the label band by name
3. Applies per-band normalization using rules from `normalization_stats.json`
4. Converts NaN label pixels to `ignore_index=255`
5. Applies label remapping if present (e.g., multiclass to binary)
6. Optional data augmentation (random flips and rotations)
7. Returns `(predictors, labels)` tensors

### Key Components

- **`WetlandPatchDataset`**: Main Dataset class
- **`create_data_splits()`**: Splits patch files 70% train / 15% validation / 15% test
- **`create_dataloaders()`**: Wraps splits into PyTorch DataLoaders

### Testing

```bash
python 02_dataset.py
```

Loads a sample batch and prints tensor shapes and value ranges to verify normalization.

---

## Step 3: Model Architecture (`03_unet_model.py`)

U-Net encoder-decoder architecture with skip connections, residual encoder blocks, and squeeze-and-excitation (SE) channel attention in the decoder. See [UNet_Architecture_Overview.md](UNet_Architecture_Overview.md) for a detailed breakdown.

### Architecture

```
Input (29 ch) → Residual Encoder (progressive downsampling) → Bottleneck → SE Decoder (upsampling + skip + attention) → Output (5 ch)
```

- **Encoder blocks**: Double Conv-BN-ReLU with residual (shortcut) connections. A 1x1 projection handles channel mismatches. Improves gradient flow through the encoder.
- **Decoder blocks**: Upsample + skip concatenation + Conv-BN-ReLU + Squeeze-and-Excitation. SE learns to reweight channels after fusing encoder and decoder features.
- **Bottleneck**: Standard double Conv-BN-ReLU (no residual, no SE).

### Configuration

| Setting | Local (Apple M1) | HPC (NVIDIA GPU) |
|---------|-------------------|-------------------|
| `base_filters` | 32 | 64 |
| `depth` | 4 | 5 |
| Filter progression | 32→64→128→256→512 | 64→128→256→512→1024 |
| Parameters (approx) | ~7.8M | ~125.3M |

- `in_channels` and `num_classes` are read from `normalization_stats.json` — not hardcoded.

### Testing

```bash
python 03_unet_model.py
```

Runs a forward pass with dummy data to verify the model builds correctly.

---

## Step 4: Train (`04_train.py`)

Full training loop with class weighting, learning rate scheduling, and model checkpointing.

### Usage

```bash
python 04_train.py \
  --patches-dir ../../Data/Training_Data/R_Patches \
  --stats-path ../../Data/Training_Data/normalization_stats.json \
  --output-dir ../../Models \
  --epochs 50 \
  --batch-size 16 \
  --lr 1e-4 \
  --base-filters 32 \
  --depth 4 \
  --workers 4 \
  --seed 42
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--patches-dir` | `Data/Training_Data/R_Patches` | Training patches directory |
| `--stats-path` | `Data/Training_Data/normalization_stats.json` | Normalization stats |
| `--output-dir` | `Models` | Where to save models and history |
| `--epochs` | 50 | Number of training epochs |
| `--batch-size` | 16 | Batch size |
| `--lr` | 1e-4 | Initial learning rate |
| `--base-filters` | 32 | U-Net base filter count |
| `--depth` | 4 | U-Net encoder/decoder depth |
| `--workers` | 4 | DataLoader worker processes (use 0 on macOS if issues arise) |
| `--seed` | 42 | Random seed for reproducibility |

### Training Details

- **Loss**: Hybrid CrossEntropy + Dice (`HybridLoss`). CE carries inverse frequency class weights and `ignore_index=255`; Dice is computed per-class on softmax probabilities then averaged (inherently class-balanced). Combined as `CE + Dice` with equal weight.
- **Optimizer**: AdamW (weight decay 1e-4)
- **Scheduler**: ReduceLROnPlateau (reduces LR when validation loss plateaus)
- **Metrics tracked**: loss, pixel accuracy, mean IoU (per epoch)
- **Checkpointing**: saves best model (lowest validation loss) and final model

### Output Files

| File | Description |
|------|-------------|
| `Models/best_model.pth` | Best checkpoint (lowest validation loss) |
| `Models/final_model.pth` | Final epoch checkpoint |
| `Models/training_history.json` | Per-epoch loss, accuracy, and IoU |

---

## Step 5: Evaluate (`05_evaluate.py`)

Runs the trained model on the held-out test set and computes detailed metrics.

### Usage

```bash
python 05_evaluate.py \
  --model ../../Models/best_model.pth \
  --patches-dir ../../Data/Training_Data/R_Patches \
  --stats-path ../../Data/Training_Data/normalization_stats.json \
  --output evaluation_metrics.json \
  --batch-size 16 \
  --base-filters 32 \
  --depth 4 \
  --seed 42
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--model` | `Models/best_model.pth` | Path to trained model checkpoint |
| `--patches-dir` | `Data/Training_Data/R_Patches` | Patches directory |
| `--stats-path` | `Data/Training_Data/normalization_stats.json` | Stats JSON |
| `--output` | None (prints to console) | Optional JSON file for saving metrics |
| `--batch-size` | 16 | Batch size for inference |
| `--base-filters` | 32 | Must match the trained model |
| `--depth` | 4 | Must match the trained model |
| `--seed` | 42 | Must match training seed (same test split) |

### Metrics Reported

- **Per-class**: precision, recall, F1 score, IoU, pixel count (support)
- **Overall**: accuracy, mean IoU, macro F1
- **Confusion matrix**: predicted vs. actual class counts

---

## Step 6: Predict (`06_predict.py`)

Applies a trained model to a new raster for wall-to-wall classification.

### Usage

```bash
python 06_predict.py \
  input_raster.tif \
  output_classification.tif \
  --model ../../Models/best_model.pth \
  --stats ../../Data/Training_Data/normalization_stats.json \
  --patch-size 128 \
  --overlap 32 \
  --base-filters 32 \
  --depth 4 \
  --probs
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `input` | *(required)* | Input multi-band raster (must have band descriptions) |
| `output` | *(required)* | Output classification raster path |
| `--model` | `Models/best_model.pth` | Trained model checkpoint |
| `--stats` | `Data/Training_Data/normalization_stats.json` | Stats JSON |
| `--patch-size` | 128 | Sliding window size (pixels) |
| `--overlap` | 32 | Overlap between adjacent windows (reduces edge artifacts) |
| `--base-filters` | 32 | Must match the trained model |
| `--depth` | 4 | Must match the trained model |
| `--probs` | False | Also save per-class probability maps |

### Output Files

| File | Format | Description |
|------|--------|-------------|
| `output_classification.tif` | uint8, 1 band | Class IDs (0–4 multiclass, 0–1 binary); 255 = unclassified |
| `output_classification.probs.tif` | float32, N bands | Per-class probabilities; N = num_classes (5 multiclass, 2 binary). Only if `--probs`. |

### How Band Matching Works

The prediction script matches bands **by name**, not position. Your input raster's band descriptions must match the names the model was trained on (e.g., "DEM", "NDVI", "VH"). Bands can be in any order, and extra bands are ignored.

---

## Interactive Notebook: wetland_pipeline.ipynb

The Jupyter notebook provides an interactive version of the full pipeline with inline visualizations.

### Running

```bash
jupyter notebook wetland_pipeline.ipynb
```

### Configuration

Edit the configuration cell near the top of the notebook:

```python
EPOCHS = 50
BATCH_SIZE = 16
LEARNING_RATE = 1e-4
BASE_FILTERS = 32      # 32 for local (M1), 64 for HPC
DEPTH = 4              # 4 for local, 5 for HPC
NUM_WORKERS = 4        # Set to 0 if issues on macOS
SEED = 42
```

### Workflow

| Cells | Step |
|-------|------|
| 1–4 | Setup, imports, configuration |
| 5–8 | Compute statistics, visualize class distributions |
| 9–14 | Create DataLoaders, inspect batches, plot sample patches |
| 15–20 | Train model with progress tracking |
| 21–25 | Evaluate on test set, confusion matrix |
| 26–30 | Predict on new rasters, visualize results |

---

## Adding a New Band

1. Include the new band in your GeoTIFF patches with a descriptive band name set in the metadata
2. If the band needs `shift_scale` or `one_hot` normalization, add an entry to `band_config.json`. If it uses standard `min_max`, no config change is needed.
3. Re-run `01_compute_statistics.py` — it discovers bands automatically
4. Re-train the model — `in_channels` updates automatically

---

## Patch Size

The pipeline is **patch-size agnostic**. The U-Net is fully convolutional and all spatial dimensions are discovered at runtime — no patch size is hardcoded in the training or dataset code.

### Changing patch size

1. Create new training patches at the desired size (e.g., 256x256) — all patches must be the same dimensions
2. Re-run `01_compute_statistics.py` on the new patches
3. Retrain the model
4. For prediction, set `--patch-size 256` (CLI) or `PATCH_SIZE = 256` (notebook) to match

### Constraint

The patch size must be divisible by **2^depth** because the U-Net encoder downsamples by 2 at each level:

| Depth | Minimum divisor | Valid sizes (examples) |
|-------|----------------|----------------------|
| 4 (default) | 16 | 128, 192, 256, 512 |
| 5 (HPC) | 32 | 128, 256, 512 |

### Where defaults live

The default patch size of 128 appears only in these locations:

| Location | What | Action needed |
|----------|------|---------------|
| `06_predict.py` `--patch-size` | CLI default for prediction sliding window | Override with `--patch-size 256` |
| `wetland_pipeline.ipynb` `PATCH_SIZE` | Notebook variable for prediction | Change to `256` |

No changes are needed in `01_compute_statistics.py`, `02_dataset.py`, `03_unet_model.py`, `04_train.py`, or `05_evaluate.py` — they all handle arbitrary patch sizes automatically.

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "No band descriptions found" | Ensure GeoTIFF band descriptions are set (use `rasterio` to verify) |
| DataLoader crashes on macOS | Set `--workers 0` (or `NUM_WORKERS=0` in notebook) |
| Out of memory during training | Reduce `--batch-size` or `--base-filters` |
| NaN values in loss | Verify `ignore_index=255` is set; check for corrupted patches. HybridLoss handles all-ignored batches gracefully. |
| Band mismatch during prediction | Input raster band descriptions must match the names in `normalization_stats.json` |
| `--base-filters` / `--depth` mismatch | Evaluation and prediction must use the same values as training |

---

## Directory Structure

```
NYS_Wetlands_DL/
├── Data/
│   ├── Training_Data/
│   │   ├── R_Patches/                  # GeoTIFF training patches (e.g., 128x128 or 256x256)
│   │   └── normalization_stats.json    # Generated by Step 1
│   └── Predictions/                    # Output from Step 6
├── Models/
│   ├── best_model.pth                  # Best checkpoint
│   ├── final_model.pth                 # Final epoch checkpoint
│   └── training_history.json           # Training metrics
└── Python_Code_Analysis/
    └── DL_Pipeline_v2/
        ├── README.md                   # This file
        ├── UNet_Architecture_Overview.md        # Detailed model architecture reference
        ├── band_config.json            # Normalization rules
        ├── band_utils.py               # Shared band utilities
        ├── 01_compute_statistics.py     # Step 1: Stats
        ├── 02_dataset.py               # Step 2: Dataset
        ├── 03_unet_model.py            # Step 3: Model
        ├── 04_train.py                 # Step 4: Train
        ├── 05_evaluate.py              # Step 5: Evaluate
        ├── 06_predict.py               # Step 6: Predict
        └── wetland_pipeline.ipynb      # Interactive notebook
```
