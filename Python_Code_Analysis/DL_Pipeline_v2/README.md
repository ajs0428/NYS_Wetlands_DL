# NYS Wetlands DL Pipeline v2 — User Guide

Deep learning pipeline for wetland semantic segmentation in New York State. Two architectures are available via `--arch`: a **U-Net** with residual blocks and SE attention (default), and **UNet3+** with full-scale skip connections and optional deep supervision. Supports two classification modes: **multiclass** (4-class: EMW, FSW, SSW, UPL) and **binary** (WET vs UPL). The mode is controlled by a single toggle in `dl_band_config.json` — both modes use the same training patches with label remapping applied at runtime.

> **Scope.** This guide covers training, evaluating, and running inference with a **single model**. The multi-config **factorial experiment** (label-provenance × below-canopy feature ablations) has its own design and run instructions in [`factorial_experiment/`](factorial_experiment/) — see [`factorial_experiment/EXECUTION.md`](factorial_experiment/EXECUTION.md). It reuses the same `dl_*` scripts but drives them through its own orchestration (`Shell_Scripts/run_*.sh`) and a different Docker mount layout; use that guide, not the [Docker / HPC Deployment](#docker--hpc-deployment) recipe below, for the experiment.

## Table of Contents

- [Quick Start](#quick-start)
- [Environment Setup](#environment-setup)
- [Glossary of Acronyms](#glossary-of-acronyms)
- [Pipeline Overview](#pipeline-overview)
- [Input Data Requirements](#input-data-requirements)
- [Configuration: dl_band_config.json](#configuration-dl_band_configjson)
- [Classification Mode: Multiclass vs Binary](#classification-mode-multiclass-vs-binary)
- [Shared Utilities: dl_band_utils.py](#shared-utilities-dl_band_utilspy)
- [Step 1: Compute Statistics](#step-1-compute-statistics-dl_01_compute_statisticspy)
- [Step 2: Dataset & DataLoaders](#step-2-dataset--dataloaders-dl_02_datasetpy)
- [Step 3: Model Architecture](#step-3-model-architecture-dl_03_unet_modelpy)
- [Step 4: Train (Lightning)](#step-4-train-lightning-dl_04_train_lightningpy)
- [Step 4 (Legacy): Train](#step-4-legacy-train-dl_04_trainpy)
- [Step 5: Evaluate](#step-5-evaluate-dl_05_evaluatepy)
- [Step 6: Predict](#step-6-predict-dl_06_predictpy)
- [Step 6b: Predict a HUC Without Building a Stack](#step-6b-predict-a-huc-without-building-a-stack-dl_06b_predict_hucpy)
- [Step 7: SHAP Band Importance](#step-7-shap-band-importance-dl_07_shap_analysispy)
- [Interactive Notebook](#interactive-notebook-wetland_pipelineipynb)
- [Loss Function: Hybrid Focal + Dice](#loss-function-hybrid-focal--dice)
- [Adding a New Band](#adding-a-new-band)
- [Patch Size](#patch-size)
- [Docker / HPC Deployment](#docker--hpc-deployment)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
# 1. Install dependencies and activate the environment
uv sync                        # from project root (NYS_Wetlands_DL/)
source .venv/bin/activate

# 2. Navigate to the pipeline directory
cd "Python_Code_Analysis/DL_Pipeline_v2"

# 3. Compute normalization statistics from training patches
python dl_01_compute_statistics.py \
  --patches-dir "../../Data/Training_Data/R_Patches"

# 4. Train the model (Lightning)
python dl_04_train_lightning.py --epochs 50 --batch-size 16

# 5. Evaluate on the held-out test set
python dl_05_evaluate.py --model "../../Models/best_multiclass_unet.ckpt" --seed 42

# 6. Run inference on a new raster
python dl_06_predict.py input_raster.tif output_classification.tif --probs
```

---

## Environment Setup

**Using uv (recommended):**

```bash
# From project root (NYS_Wetlands_DL/)
uv sync                        # Mac/CPU — auto-detects MPS (includes SHAP)
source .venv/bin/activate

# On HPC with CUDA:
uv sync --extra-index-url https://download.pytorch.org/whl/cu121

# For Jupyter notebook support (ipykernel):
uv sync --extra notebooks
```

**Using conda (legacy):**

```bash
conda activate wetland-cnn
```

Key dependencies: PyTorch, Lightning, rasterio, NumPy, scikit-learn, matplotlib. All managed via `pyproject.toml` at the project root.

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

### Remote Sensing Bands — Terrain & Landform

| Acronym | Full Name | Description | In Current Patches? |
|---------|-----------|-------------|---------------------|
| DEM | Digital Elevation Model | Ground surface elevation (meters) | Yes |
| slope | Slope | Surface gradient in degrees | Yes (slope_local) |
| Geomorph | Geomorphon | Landform classification (10 categories: flat, peak, ridge, shoulder, spur, slope, hollow, footslope, valley, pit). One-hot encoded to 10 channels. | Yes (Geomorph_local) |
| flowacc | Flow Accumulation | Number of upslope cells draining through each cell; highlights drainage networks | Yes |
| twi | Topographic Wetness Index | ln(flow accumulation / tan(slope)); higher = wetter, flow-converging terrain | Yes |
| CHM | Canopy Height Model | Vegetation height above ground (meters), derived from LiDAR | Yes |
| TPI | Topographic Position Index | Relative elevation compared to surrounding area | No (removed in a prior iteration) |
| meanc | Mean Curvature | Average surface curvature (concavity/convexity) | No (removed) |
| planc | Plan Curvature | Curvature perpendicular to slope direction | No (removed) |
| profc | Profile Curvature | Curvature in the direction of slope | No (removed) |
| dmv | Deviation from Mean Value | Local deviation of elevation from the neighborhood mean | No (removed) |

### Remote Sensing Bands — Canopy / Vegetation Structure (LiDAR)

| Acronym | Full Name | Description | In Current Patches? |
|---------|-----------|-------------|---------------------|
| pct_below_1m | Returns below 1 m | Fraction of LiDAR returns below 1 m — low/ground vegetation density | Yes |
| pct_1m_to_5m | Returns 1–5 m | Fraction of LiDAR returns between 1 m and 5 m — shrub/understory density | Yes |
| pct_above_5m | Returns above 5 m | Fraction of LiDAR returns above 5 m — overstory/canopy density | Yes |

### Remote Sensing Bands — NAIP Imagery

NAIP is captured in two seasonal states. Both sets are 4-band (R/G/B/NIR), 0–255, on the same scale.

| Acronym | Full Name | Description | In Current Patches? |
|---------|-----------|-------------|---------------------|
| r, g, b, nir | NAIP leaf-on | Red / Green / Blue / Near-Infrared, leaf-on (growing season) | Yes |
| r_lo, g_lo, b_lo, nir_lo | NAIP leaf-off | Red / Green / Blue / Near-Infrared, leaf-off ortho (dormant season) | Yes |
| n_ndvi | NAIP NDVI | (nir - r) / (nir + r), derived from NAIP imagery | No (removed) |
| n_ndwi | NAIP NDWI | (g - nir) / (g + nir), derived from NAIP imagery | No (removed) |

### Remote Sensing Bands — Removed Spectral Indices & SAR

These were used in earlier iterations but are **not** in the current patches.

| Acronym | Full Name | What It Measures | In Current Patches? |
|---------|-----------|------------------|---------------------|
| EVI | Enhanced Vegetation Index | Vegetation with atmospheric correction | No (removed) |
| NDYI | Normalized Difference Yellowness Index | Vegetation senescence / yellow coloring | No (removed) |
| GDVI | Green Difference Vegetation Index | Green vegetation density | No (removed) |
| VV | SAR Vertical-Vertical Polarization | Surface/structure backscatter | No (removed) |
| VH | SAR Vertical-Horizontal Polarization | Volume scattering, sensitive to vegetation | No (removed) |

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
GeoTIFF Patches (18 bands: 17 predictors + 1 label -> 26 model input channels)
        |
        v
 +---------------------+
 | dl_01_compute_stats  | -> normalization_stats.json
 +---------------------+
        |
        v
 +---------------------+
 | dl_02_dataset        | -> PyTorch DataLoaders (train / val / test)
 +---------------------+
        |
        v
 +---------------------+
 | dl_03_unet_model     | -> U-Net architecture (residual blocks + SE attention)
 | dl_03_unet3plus_model| -> UNet3+ (full-scale skips + deep supervision)
 |  (selected by --arch via dl_model_factory.build_net)
 +---------------------+
        |
        v
 +---------------------------------+
 | dl_04_train_lightning            | -> best_{mode}_{arch}_*.ckpt (Lightning checkpoints)
 |  (or dl_04_train.py legacy)     |    + CSV/TensorBoard logs
 +---------------------------------+
        |
        v
 +---------------------+
 | dl_05_evaluate       | -> Per-class metrics, confusion matrix
 +---------------------+
        |
        v
 +---------------------+
 | dl_06_predict        | -> Classification GeoTIFF + probability maps
 +---------------------+
        |
        v
 +---------------------+
 | dl_07_shap_analysis  | -> Per-band SHAP importance plots + JSON (Models/SHAP/)
 +---------------------+

Shared modules: dl_losses.py (FocalLoss, DiceLoss, HybridLoss), dl_model_utils.py (checkpoint loading),
                dl_model_factory.py (build_net architecture dispatch), dl_band_utils.py (band discovery/config)
```

---

## Input Data Requirements

Training patches are GeoTIFF files located in `Data/Training_Data/R_Patches/`. The current patches are 256x256 pixels, but the pipeline supports any square patch size (see [Patch Size](#patch-size) below).

Each patch contains 18 bands (17 predictors + 1 label). Band names are stored in the GeoTIFF band descriptions and are discovered at runtime — no hardcoded indices. After one-hot expansion of `Geomorph_local` (1 band -> 10 channels), the model receives **26 input channels**.

**Current band layout (613 patches):**

| Band # | Band Name | Category |
|--------|-----------|----------|
| 1 | DEM | Terrain |
| 2 | slope_local | Terrain |
| 3 | Geomorph_local | Landform (one-hot -> 10 channels) |
| 4 | flowacc | Terrain |
| 5 | twi | Terrain |
| 6 | CHM | Canopy height |
| 7 | r | NAIP leaf-on |
| 8 | g | NAIP leaf-on |
| 9 | b | NAIP leaf-on |
| 10 | nir | NAIP leaf-on |
| 11 | r_lo | NAIP leaf-off |
| 12 | g_lo | NAIP leaf-off |
| 13 | b_lo | NAIP leaf-off |
| 14 | nir_lo | NAIP leaf-off |
| 15 | pct_below_1m | Veg structure |
| 16 | pct_1m_to_5m | Veg structure |
| 17 | pct_above_5m | Veg structure |
| 18 | MOD_CLASS | Label |

> **Channel count:** 17 predictor bands, but `Geomorph_local` one-hot expands 1 -> 10 channels, so `in_channels = 16 + 10 = 26`. This is the value stored in `normalization_stats.json` and read by the model.

**Classes (multiclass mode):**

| Value | Code | Description |
|-------|------|-------------|
| 0 | EMW | Emergent Wetland |
| 1 | FSW | Forested Wetland |
| 2 | SSW | Scrub-Shrub Wetland |
| 3 | UPL | Upland / Background |
| 255 | — | Unlabeled (excluded from training) |

> **Note:** Open Water Wetland (OWW) has been removed from the current classification. OWW patches were excluded from training data because open water is reliably detectable via spectral indices (MNDWI, NDWI) and does not require a deep learning model. OWW can be reintroduced by adding it back to `class_names` in `dl_band_config.json` and regenerating training patches that include OWW labels.

**Classes (binary mode):** EMW/FSW/SSW are remapped to WET (0), UPL stays as UPL (1). See [Classification Mode](#classification-mode-multiclass-vs-binary).

---

## Configuration: dl_band_config.json

The single file to edit when changing band normalization. Located alongside the scripts.

```json
{
  "label_band": "MOD_CLASS",
  "classification_mode": "multiclass",
  "binary_mapping": {
    "WET": ["EMW", "FSW", "SSW"],
    "UPL": ["UPL"]
  },
  "default_method": "min_max",
  "band_normalization": {
    "EVI":   {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "NDYI":  {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "GDVI":  {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "n_ndvi": {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "n_ndwi": {"method": "shift_scale", "shift": 1.0, "scale": 2.0},
    "Geomorph_local": {"method": "one_hot", "num_classes": 10, "class_range": [1, 10]}
  },
  "class_names": ["EMW", "FSW", "SSW", "UPL"],
  "ignore_index": 255
}
```

> **Note:** The `EVI`, `NDYI`, `GDVI`, `n_ndvi`, and `n_ndwi` entries are **dead config** — those bands are no longer in the current patches, so the rules are simply ignored (listed-but-absent bands have no effect). They are kept so the spectral indices normalize correctly if re-added. The only entry that matches a current band is `Geomorph_local` (one-hot); all other current bands fall back to `min_max`.

**Normalization methods:**

| Method | Formula | Use Case |
|--------|---------|----------|
| `min_max` | (x - min) / (max - min) -> [0, 1] | Default for continuous bands (DEM, CHM, slope, SAR, etc.) |
| `shift_scale` | (x + shift) / scale -> [0, 1] | Spectral indices with known range (e.g., NDVI: [-1, 1]) |
| `one_hot` | Encode to N binary channels | Categorical bands (e.g., Geomorph if present) |

Any band **not listed** in `band_normalization` automatically uses `min_max`. You only need to add entries for `shift_scale` or `one_hot` bands.

---

## Classification Mode: Multiclass vs Binary

The pipeline supports two classification modes, controlled by `classification_mode` in `dl_band_config.json`:

| Mode | Classes | Output Bands | Use Case |
|------|---------|--------------|----------|
| `"multiclass"` | EMW, FSW, SSW, UPL (4) | 4 | Fine-grained wetland type mapping |
| `"binary"` | WET, UPL (2) | 2 | Wetland presence/absence detection |

### Switching Modes

1. Edit `classification_mode` in `dl_band_config.json` to `"binary"` or `"multiclass"`
2. Re-run `dl_01_compute_statistics.py` to regenerate `normalization_stats.json`
3. Train, evaluate, and predict as usual — all downstream scripts adapt automatically

### How It Works

- The `binary_mapping` field in `dl_band_config.json` defines how original classes group into binary classes. The key order determines integer encoding (WET=0, UPL=1).
- `dl_01_compute_statistics.py` builds a `label_remap` dict (e.g., `{0:0, 1:0, 2:0, 3:0, 4:1}`) and aggregates class counts under the binary labels. Both `label_remap` and `classification_mode` are stored in `normalization_stats.json`.
- `dl_02_dataset.py` reads `label_remap` from the stats and applies it on-the-fly via a vectorized numpy lookup table. The original training patches are never modified.
- Downstream scripts (`dl_04_train`, `dl_05_evaluate`, `dl_06_predict`) derive `num_classes` from `len(stats["class_names"])`, so they work with either 2 or 4 classes without any code changes.

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

## Shared Utilities: dl_band_utils.py

Imported by all pipeline scripts. Key functions:

| Function | Purpose |
|----------|---------|
| `load_band_config(path)` | Load `dl_band_config.json` |
| `discover_bands_from_raster(path)` | Read band names from GeoTIFF descriptions |
| `get_predictor_band_names(names, label)` | Return band names excluding the label band |
| `get_normalization_method(band, config)` | Look up a band's normalization (with default fallback) |
| `compute_in_channels(names, config)` | Count total input channels after one-hot expansion |
| `compute_in_channels_from_stats(path)` | Read `in_channels` from `normalization_stats.json` |
| `validate_prediction_bands(raster, expected, label)` | Match raster bands to expected predictors by name |

---

## Step 1: Compute Statistics (`dl_01_compute_statistics.py`)

Scans all training patches to compute per-band normalization statistics and class frequencies. Produces `normalization_stats.json`, which is the single source of truth for all downstream scripts.

### Usage

```bash
python dl_01_compute_statistics.py \
  --patches-dir ../../Data/Training_Data/R_Patches \
  --output ../../Data/Training_Data/normalization_stats.json \
  --config dl_band_config.json
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--patches-dir` | `Data/Training_Data/R_Patches` | Directory containing GeoTIFF training patches |
| `--output` | `Data/Training_Data/normalization_stats.json` | Output path for the stats JSON |
| `--config` | Auto-detected | Path to `dl_band_config.json` |

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
  "num_patches": 613,
  "in_channels": 26,
  "label_band": "MOD_CLASS",
  "predictor_names": ["DEM", "slope_local", "Geomorph_local", "flowacc", "twi", "..."],
  "normalization": {
    "DEM": {"method": "min_max", "min": 98.37, "max": 652.21},
    "Geomorph_local": {"method": "one_hot", "num_classes": 10, "class_range": [1, 10]}
  },
  "class_counts": {"EMW": 1049074, "FSW": 2119191, "SSW": 904329, "UPL": 11852654},
  "class_weights": {"EMW": 11.3, "FSW": 5.59, "SSW": 13.11, "UPL": 1.0}
}
```

---

## Step 2: Dataset & DataLoaders (`dl_02_dataset.py`)

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
python dl_02_dataset.py
```

Loads a sample batch and prints tensor shapes and value ranges to verify normalization.

---

## Step 3: Model Architecture (`dl_03_unet_model.py`, `dl_03_unet3plus_model.py`)

Two architectures are available, selected with `--arch` on the train/evaluate/predict scripts. `dl_model_factory.build_net(arch, ...)` is the single dispatch point; each architecture ignores the flags meant for the other. `--arch` defaults to `unet`, so existing workflows are unchanged.

- **`unet`** (`dl_03_unet_model.py`) — the default U-Net described below.
- **`unet3plus`** (`dl_03_unet3plus_model.py`) — UNet3+ with full-scale skip connections and optional deep supervision (see [UNet3+](#unet3) below).

### U-Net Architecture

U-Net encoder-decoder architecture with skip connections, residual encoder blocks, and squeeze-and-excitation (SE) channel attention in the decoder. See [UNet_Architecture_Overview.md](UNet_Architecture_Overview.md) for a detailed breakdown.

```
Input (26 ch) -> Residual Encoder (progressive downsampling) -> Bottleneck -> SE Decoder (upsampling + skip + attention) -> Output (4 ch)
```

- **Encoder blocks**: Double Conv-BN-ReLU with residual (shortcut) connections. A 1x1 projection handles channel mismatches. Improves gradient flow through the encoder.
- **Decoder blocks**: Upsample + skip concatenation + Conv-BN-ReLU + Squeeze-and-Excitation. SE learns to reweight channels after fusing encoder and decoder features.
- **Bottleneck**: Standard double Conv-BN-ReLU (no residual, no SE).
- **Optional ASPP**: Atrous Spatial Pyramid Pooling module after the bottleneck (`--use-aspp`). Uses parallel dilated convolutions at multiple rates (default 6/12/18) plus global average pooling to expand the receptive field to ~250m+ at 1m resolution. Off by default for backward compatibility. Use `--aspp-rates 3 6 12` for depth=5 (smaller bottleneck spatial dims).

### Configuration

| Setting | Local (Apple M1) | HPC (NVIDIA GPU) |
|---------|-------------------|-------------------|
| `base_filters` | 32 | 64 |
| `depth` | 4 | 5 |
| Filter progression | 32->64->128->256->512 | 64->128->256->512->1024 |
| Parameters (approx) | ~7.8M | ~125.3M |

- `in_channels` and `num_classes` are read from `normalization_stats.json` — not hardcoded.

### Testing

```bash
python dl_03_unet_model.py
```

Runs a forward pass with dummy data to verify the model builds correctly.

### UNet3+

UNet3+ (`--arch unet3plus`) replaces the U-Net's single same-level skip with **full-scale skip connections**: every decoder node aggregates feature maps from *all* encoder scales (downsampled) and *all* deeper decoder nodes plus the bottleneck (upsampled). Each source is unified to a common width (`--cat-channels`, default 64) before concatenation, so every decoder node is `cat_channels * (depth + 1)` wide (320 at the defaults). It reuses the same residual `ConvBlock` and `SqueezeExcitation` building blocks as the U-Net.

```
Input -> Residual Encoder -> Bottleneck
                |  \  \  \
                v   v  v  v   (each decoder node pulls from every scale)
            Full-scale-skip Decoder -> Output (+ deep-supervision side heads)
```

- **Full-scale skips**: improve multi-scale boundary delineation; more memory-intensive than the plain U-Net (every decoder node concatenates `depth + 1` unified branches).
- **Deep supervision** (`--deep-supervision`): attaches a classifier head to every decoder stage and the bottleneck. During training the model returns a list of full-resolution heads and the loss is summed over all of them (metrics use the main/finest head). In `eval()` it returns a single tensor, so evaluation and prediction are unchanged.
- **Output contract**: single tensor in `eval()`, list of heads in `train()` when deep supervision is on. `dl_04_train_lightning.py`'s `_shared_step` handles both transparently.

Parameters: ~15M at `base_filters=32`, `depth=4` (vs ~7.8M for the plain U-Net at the same settings). On HPC (`depth=5`, `base_filters=64`) expect to use `--precision 16-mixed` and/or a smaller `--batch-size` to fit memory.

Test it with:

```bash
python dl_03_unet3plus_model.py
```

---

## Step 4: Train (Lightning) (`dl_04_train_lightning.py`)

Primary training script using PyTorch Lightning. Provides automatic checkpointing, early stopping, LR monitoring, and progress bars.

### Usage

```bash
python dl_04_train_lightning.py \
  --patches-dir ../../Data/Training_Data/R_Patches \
  --stats-path ../../Data/Training_Data/normalization_stats.json \
  --output-dir ../../Models \
  --epochs 50 \
  --batch-size 16 \
  --lr 1e-4 \
  --base-filters 32 \
  --depth 4 \
  --workers 4 \
  --seed 420 \
  --early-stopping 15
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--patches-dir` | `Data/Training_Data/R_Patches` | Training patches directory |
| `--stats-path` | `Data/Training_Data/normalization_stats.json` | Normalization stats |
| `--output-dir` | `Models` | Where to save checkpoints and logs |
| `--epochs` | 50 | Maximum training epochs |
| `--batch-size` | 16 | Batch size |
| `--lr` | 1e-4 | Initial learning rate |
| `--arch` | `unet` | Architecture: `unet` or `unet3plus` |
| `--base-filters` | 32 | Base filter count |
| `--depth` | 4 | Encoder/decoder depth |
| `--workers` | 4 | DataLoader worker processes (use 0 on macOS if issues arise) |
| `--seed` | None | Random seed for reproducibility |
| `--early-stopping` | 15 | Early stopping patience (epochs without improvement) |
| `--use-aspp` | False | [unet] Add ASPP module at the bottleneck |
| `--aspp-rates` | `6 12 18` | [unet] Dilation rates for ASPP branches (space-separated) |
| `--cat-channels` | 64 | [unet3plus] Unified channels per skip branch |
| `--deep-supervision` | False | [unet3plus] Add a loss head to every decoder stage + bottleneck |
| `--ce-weight` | 1.0 | Weight for Focal Loss component |
| `--dice-weight` | 1.0 | Weight for Dice Loss component |
| `--focal-gamma` | 2.0 | Focal Loss gamma (0 = plain CE, 2 = standard focal) |
| `--label-smoothing` | 0.0 | Label smoothing factor (0.0 = off) |
| `--kfold` | 0 | Number of cross-validation folds (0 = disabled) |

### Training Details

- **Loss**: Hybrid Focal + Dice (`HybridLoss` in `dl_losses.py`). Focal Loss replaces plain CrossEntropy — it applies a `(1 - p_t)^gamma` modulation that down-weights easy/well-classified pixels (mostly the dominant UPL class) and focuses training on hard examples (minority wetland classes, boundary pixels). Class weights from inverse frequency are still applied. Dice is computed per-class on softmax probabilities then averaged (inherently class-balanced). Default combination: `0.5 * Focal + 1.0 * Dice`.
- **Optimizer**: AdamW (weight decay 1e-4)
- **Scheduler**: ReduceLROnPlateau (reduces LR when validation loss plateaus)
- **Callbacks**: ModelCheckpoint (best val/loss), EarlyStopping, LearningRateMonitor
- **Metrics logged**: train/loss, train/acc, train/iou, val/loss, val/acc, val/iou (via Lightning's built-in logging)
- **Device**: Auto-detected (MPS on Mac, CUDA on Linux, CPU fallback)

### Output Files

| File | Description |
|------|-------------|
| `Models/best_{mode}_{arch}_bf{bf}_d{depth}_{timestamp}.ckpt` | Best Lightning checkpoint (lowest validation loss); a matching `.safetensors` + `.meta.json` is auto-exported |
| `Models/lightning_logs/` | CSV logs and optional TensorBoard logs |

The architecture and its hyperparameters (`arch`, `cat_channels`, `deep_supervision`, etc.) are stored in the checkpoint, the `.meta.json` sidecar, and `training_log.json`, so downstream scripts reconstruct the right model automatically.

### Key Components

- **`WetlandDataModule`**: Wraps `create_dataloaders()` from `dl_02_dataset.py` as a Lightning data module
- **`WetlandSegmentationModule`**: Lightning module wrapping the segmentation backbone (U-Net or UNet3+); `_shared_step` handles both a single output tensor and the list of heads produced under deep supervision
- **`train()`**: Entry point that wires up data, model, callbacks, and Trainer

---

## Step 4 (Legacy): Train (`dl_04_train.py`)

Manual training loop kept as a reference and fallback. Same loss, optimizer, and training logic as the Lightning version but without automatic callbacks.

### Usage

```bash
python dl_04_train.py --epochs 50 --batch-size 16
```

### Output Files

| File | Description |
|------|-------------|
| `Models/best_model_{mode}.pth` | Best checkpoint (lowest validation loss) |
| `Models/final_model_{mode}.pth` | Final epoch checkpoint |
| `Models/training_history_{mode}.json` | Per-epoch loss, accuracy, and IoU |

> **Note:** Both legacy `.pth` and Lightning `.ckpt` checkpoints are supported by `dl_05_evaluate.py` and `dl_06_predict.py` via `dl_model_utils.py`.

---

## Step 5: Evaluate (`dl_05_evaluate.py`)

Runs the trained model on the held-out test set and computes detailed metrics.

### Usage

```bash
python dl_05_evaluate.py \
  --model ../../Models/best_multiclass_unet.ckpt \
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
| `--base-filters` | 32 | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--depth` | 4 | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--seed` | 42 | Must match training seed (same test split) |
| `--use-aspp` | False | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--aspp-rates` | `6 12 18` | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |

> The architecture itself (`unet` vs `unet3plus`) and its params (`cat_channels`, `deep_supervision`) are read from the checkpoint's hparams / `.meta.json` sidecar — no `--arch` flag is needed when evaluating a `.ckpt` or `.safetensors` model.

### Metrics Reported

- **Per-class**: precision, recall, F1 score, IoU, pixel count (support)
- **Overall**: accuracy, mean IoU, macro F1
- **Confusion matrix**: predicted vs. actual class counts

---

## Step 6: Predict (`dl_06_predict.py`)

Applies a trained model to a new raster for wall-to-wall classification.

### Usage

```bash
python dl_06_predict.py \
  input_raster.tif \
  output_classification.tif \
  --model ../../Models/best_multiclass_unet.ckpt \
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
| `--base-filters` | 32 | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--depth` | 4 | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--probs` | False | Also save per-class probability maps |
| `--use-aspp` | False | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--aspp-rates` | `6 12 18` | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |

> Architecture (`unet`/`unet3plus`) and its params are read from the checkpoint, so a UNet3+ model predicts with the same command — just point `--model` at its checkpoint.

### Output Files

| File | Format | Description |
|------|--------|-------------|
| `output_classification.tif` | uint8, 1 band | Class IDs (0-3 multiclass, 0-1 binary); 255 = unclassified |
| `output_classification.probs.tif` | float32, N bands | Per-class probabilities; N = num_classes (4 multiclass, 2 binary). Only if `--probs`. |

### How Band Matching Works

The prediction script matches bands **by name**, not position. Your input raster's band descriptions must match the names the model was trained on (e.g., "DEM", "Geomorph_local", "nir_lo"). Bands can be in any order, and extra bands are ignored.

---

## Step 6b: Predict a HUC Without Building a Stack (`dl_06b_predict_huc.py`)

`dl_06_predict.py` needs one pre-stacked multi-band GeoTIFF as input. Building those `*_stack.tif` files for every HUC duplicates a lot of data (coarse layers get resampled up to the 1 m DEM grid, so a stack is often *larger* than its sources) and means rsyncing a fresh stack for every HUC and every model run.

`dl_06b_predict_huc.py` avoids that entirely. It assembles the predictor stack **in memory** from the canonical source rasters and feeds it to the same sliding-window inference loop — nothing is ever written to disk as an intermediate stack.

### How it works

- **`dl_huc_stack.py`** is the Python port of `R_Code_Analysis/huc_stack.R` — the single source of truth for the band recipe. `VirtualHucStack` opens the DEM as the reference grid, wraps any mismatched source (NAIP, ortho) in a `rasterio.WarpedVRT` (a virtual, on-read resample — the equivalent of `terra::resample(r, ref)`), and bakes in the same transforms: `log(flowacc)`, drop `TPI_*` and NAIP `ndvi`/`ndwi`, `_lo` suffix on ortho. Categorical bands (`Geomorph_local`) are warped nearest-neighbour so they are never interpolated.
- The virtual stack exposes the same surface a rasterio dataset does (`.descriptions`, `.profile`, `.nodata`, `.read(indexes, window=…)`), so `predict_from_source()` reads it **window by window**. Memory stays bounded by a single tile regardless of HUC size, and band selection is by name (the same `validate_prediction_bands` contract as Step 6).
- Coverage is wall-to-wall: tiles overlap, the final row/column is snapped to the raster edge, and overlapping predictions are blended with a Hanning weight before `argmax`. The output is one seamless classification raster the same width × height as the DEM.

### Inspect the contract first (no model needed)

Before predicting, confirm the source files resolve and the assembled bands match what the model expects:

```bash
python dl_huc_stack.py \
  --huc 041402011002 --cluster 208 \
  --data-root /scratch/NYS_Wetlands_Data \
  --inspect
```

This prints the resolved source file per dataset and the full ordered band list. Spot-check that the 17 predictor names from your `normalization_stats.json` are present.

### Usage

```bash
python dl_06b_predict_huc.py \
  --huc 041402011002 --cluster 208 \
  --data-root /scratch/NYS_Wetlands_Data \
  --model ../../Models/best_binary_unet.safetensors \
  --stats ../../Data/Training_Data/binary_normalization_stats.json \
  --out-dir ../../Data/HUC_DL_Predictions \
  --patch-size 128 --overlap 64 --probs
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--huc` | *(required)* | HUC id (e.g. `041402011002`) |
| `--cluster` | *(required)* | Cluster number (e.g. `208`) |
| `--data-root` | `$NYS_WETLANDS_DATA_ROOT` or sibling `NYS_Wetlands_Data/` | Root of the data project holding the source rasters |
| `--model` | `Models/best_model.safetensors` | Trained model checkpoint |
| `--stats` | mode-specific `*_normalization_stats.json` | Stats JSON (defines the predictor-name contract) |
| `--out-dir` | `Data/HUC_DL_Predictions` | Output directory |
| `--patch-size` / `--overlap` | 128 / 64 | Sliding-window size and overlap |
| `--base-filters` / `--depth` | 32 / 4 | Auto-detected from `.ckpt`/`.safetensors`; fallback for legacy `.pth` |
| `--probs` | False | Also save per-class probability maps |
| `--use-aspp` / `--aspp-rates` | False / `6 12 18` | Auto-detected from checkpoint |

Output is named `DLpred_<mode>_cluster_<cluster>_huc_<huc>.tif` under `--out-dir`.

### Getting the source rasters to the GPU server

The source datasets (DEM, terrain, hydro, CHM, NAIP, ortho, lidar) must be reachable at `--data-root`. Sync **just the files for the HUC(s) you're mapping** with `rsync_huc_sources.sh` (project root `Shell_Scripts/`), which mirrors the directory layout `dl_huc_stack.py` expects:

```bash
SERVER="user@hpc.example.edu:" REMOTE_ROOT="/projects/NYS_Wetlands_Data" \
LOCAL_ROOT="/scratch/NYS_Wetlands_Data" \
  ./rsync_huc_sources.sh 208 041402011002
```

You sync the sources **once** and reuse them for every model — there is no per-run stack to rebuild or transfer.

### Band-recipe parity

`dl_huc_stack.py` must stay in lock-step with `huc_stack.R`. If the R recipe changes (a new transform, drop, or rename), mirror it in the Python port. Use `--inspect` against a HUC you also have an old stack for to diff band names and confirm parity.

---

## Step 7: SHAP Band Importance (`dl_07_shap_analysis.py`)

Computes per-band importance using `shap.GradientExplainer` to understand which input features the trained model relies on. Uses a wrapper that spatially averages the segmentation output so the explainer sees per-class scalar outputs.

### Usage

```bash
# Single model
python dl_07_shap_analysis.py --model ../../Models/best_multiclass_unet.ckpt

# Custom output directory, more background/test samples
python dl_07_shap_analysis.py \
  --model ../../Models/best_multiclass_bf64_d4_20260420_2239.safetensors \
  --output-dir ../../Models/SHAP \
  --n-background 100 --n-test 30 --crop-size 128
```

A shell script wrapper (`Shell_Scripts/DL_model_shap_HPC.sh`) runs the analysis for a single checkpoint, a k-fold directory, or a list of models.

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--model` | *(required)* | Path to `.ckpt`, `.safetensors`, or `.pth` checkpoint |
| `--patches-dir` | `Data/Training_Data/R_Patches` | Patches directory |
| `--stats-path` | `Data/Training_Data/normalization_stats.json` | Stats JSON (used for band normalization only) |
| `--output-dir` | `Models/SHAP` | Where plots and JSON are written |
| `--seed` | 420 | Data-split seed (match training seed to reproduce the test split) |
| `--n-background` | 50 | Training patches used as the SHAP background distribution |
| `--n-test` | 20 | Test patches explained |
| `--crop-size` | 128 | Center-crop each patch to reduce memory (`0` to disable) |
| `--base-filters`, `--depth`, `--use-aspp`, `--aspp-rates` | — | Fallbacks only; architecture is auto-detected from the checkpoint |

### Outputs

All files are prefixed with the checkpoint stem so multiple models can be compared side-by-side in the same directory:

| File | Description |
|------|-------------|
| `{stem}_shap_band_importance.png` | Overall per-band importance (mean \|SHAP\|) |
| `{stem}_shap_band_importance_by_class.png` | Per-class grouped bar chart |
| `{stem}_shap_summary_plot.png` | SHAP beeswarm summary |
| `{stem}_shap_importance.json` | Ranked importance values (overall + per-class) for programmatic comparison |

### Stats file and classification mode

`class_names` and `num_classes` are read from the checkpoint's own metadata (`.meta.json` sidecar for safetensors, `hyper_parameters` for Lightning `.ckpt`). Because the normalization rules and predictor bands are identical across binary and multiclass stats files, a single `normalization_stats.json` can serve both modes — you do **not** need to regenerate stats when switching between binary and multiclass models as long as the input bands are unchanged.

### Notes

- `shap.GradientExplainer` backpropagates through the model on whatever device the model is on (GPU if available), so the GPU accelerates the dominant cost. The aggregation and plotting steps are CPU/NumPy.
- One-hot-encoded bands (e.g., `Geomorph_local`) are aggregated back to a single band-level importance value by summing contributions across their expanded channels.

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

# ASPP at U-Net bottleneck (expands receptive field to ~250m+)
USE_ASPP = False            # Set True to enable
ASPP_RATES = (6, 12, 18)   # Dilation rates; use (3, 6, 12) for depth=5

# Loss parameters
CE_WEIGHT = 0.5        # Weight for Focal Loss component
DICE_WEIGHT = 1.0      # Weight for Dice Loss component
FOCAL_GAMMA = 2.0      # 0 = plain CE, 2 = standard focal
LABEL_SMOOTHING = 0.0  # 0.0 = off
```

### Workflow

| Cells | Step |
|-------|------|
| 1-4 | Setup, imports, configuration |
| 5-8 | Compute statistics, visualize class distributions |
| 9-14 | Create DataLoaders, inspect batches, plot sample patches |
| 15-20 | Train model with progress tracking |
| 21-25 | Evaluate on test set, confusion matrix |
| 26-30 | Predict on new rasters, visualize results |

---

## Loss Function

The pipeline supports several loss configurations via `dl_losses.py`, controlled by four parameters: `--ce-weight`, `--dice-weight`, `--focal-gamma`, and `--label-smoothing`. All configurations apply inverse-frequency **class weights** to handle the severe class imbalance (UPL ~74% vs. wetland classes at 6-13%) and use `ignore_index=255` to exclude unlabeled pixels.

### Weighted Cross-Entropy (recommended)

Plain weighted cross-entropy has produced the best results so far, outperforming hybrid loss configurations in both overall accuracy and IoU. To use it:

```
--ce-weight 1.0 --dice-weight 0.0 --focal-gamma 0 --label-smoothing 0.0
```

Or in the notebook:

```python
CE_WEIGHT = 1.0
DICE_WEIGHT = 0.0
FOCAL_GAMMA = 0
LABEL_SMOOTHING = 0.0
```

The class weights (computed from training data pixel counts) handle imbalance directly — minority wetland classes receive higher weight in the loss so the model doesn't collapse to predicting UPL everywhere.

### Focal Loss (optional)

Focal Loss adds a modulating factor `(1 - p_t)^gamma` that **down-weights easy, well-classified pixels** and focuses training on hard examples:

```
FL(p_t) = -alpha_t * (1 - p_t)^gamma * log(p_t)
```

| `gamma` | Effect |
|---------|--------|
| 0 | Plain CrossEntropy (no modulation) |
| 1 | Mild down-weighting of easy examples |
| 2 | Standard focal loss — a pixel classified at 95% confidence contributes only 0.25% of its original loss |
| 3+ | More aggressive; may under-weight even moderately confident predictions |

Enable by setting `--focal-gamma 2` (or higher). Note that Focal Loss applies class weights on top of its difficulty modulation, creating a combined pixel-level and class-level correction.

### Dice Loss (optional)

Dice Loss is computed per-class then averaged, making it **inherently class-balanced** — each class contributes equally regardless of pixel count. It directly optimizes the overlap (IoU-like) between predicted and true segmentation masks. Enable by setting `--dice-weight` to a positive value.

### Hybrid Focal + Dice (optional)

Combines both components:

```
total_loss = ce_weight * FocalLoss + dice_weight * DiceLoss
```

**Caution:** In testing, hybrid loss has underperformed plain weighted CE on this dataset. Likely causes include: (1) Dice loss gradient noise from rare classes that may be absent from individual batches, (2) redundant imbalance correction (class weights in Focal + inherent balance in Dice), and (3) conflicting gradient directions between the two objectives. If experimenting with hybrid loss, consider starting with a low Dice weight (e.g., `--dice-weight 0.3`) to use it as a regularizer rather than a co-equal objective.

### Tuning Guidelines

| Goal | Adjustment |
|------|-----------|
| Best tested performance | `--ce-weight 1.0 --dice-weight 0.0 --focal-gamma 0` (plain weighted CE) |
| More focus on minority classes | Increase class weights in training data, or try `--focal-gamma 2` |
| Experiment with Dice regularization | `--dice-weight 0.3 --ce-weight 1.0` |
| Prevent overconfident predictions | Add `--label-smoothing 0.05` |
| More aggressive hard-example mining | Increase `--focal-gamma` to 3 or higher |

---

## Adding a New Band

1. Include the new band in your GeoTIFF patches with a descriptive band name set in the metadata
2. If the band needs `shift_scale` or `one_hot` normalization, add an entry to `dl_band_config.json`. If it uses standard `min_max`, no config change is needed.
3. Re-run `dl_01_compute_statistics.py` — it discovers bands automatically
4. Re-train the model — `in_channels` updates automatically

---

## Patch Size

The pipeline is **patch-size agnostic**. The U-Net is fully convolutional and all spatial dimensions are discovered at runtime — no patch size is hardcoded in the training or dataset code.

### Changing patch size

1. Create new training patches at the desired size (e.g., 256x256) — all patches must be the same dimensions
2. Re-run `dl_01_compute_statistics.py` on the new patches
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
| `dl_06_predict.py` `--patch-size` | CLI default for prediction sliding window | Override with `--patch-size 256` |
| `wetland_pipeline.ipynb` `PATCH_SIZE` | Notebook variable for prediction | Change to `256` |

No changes are needed in `dl_01_compute_statistics.py`, `dl_02_dataset.py`, `dl_03_unet_model.py`, `dl_04_train.py`, or `dl_05_evaluate.py` — they all handle arbitrary patch sizes automatically.

---

## Docker / HPC Deployment

The pipeline can be run in a Docker container for reproducible GPU training on HPC clusters. The `Dockerfile` at the project root builds an image with all dependencies pre-installed.

> **This section is the single-model recipe** — one training run via the image's default pipeline `CMD`, mounting only `Data/` and `Models/`. For the **factorial experiment**, do not use this recipe: it mounts the whole repo (so edited orchestration code, per-config `stats/`, and the `results/` tree all persist on `/workdir`) and overrides the `CMD` with `run_factorial.sh`. See [`factorial_experiment/EXECUTION.md`](factorial_experiment/EXECUTION.md).

### Building the Image

Build on your local machine targeting linux/amd64 (required for HPC GPU nodes, even when building from Apple Silicon):

```bash
cd NYS_Wetlands_DL/
docker build --platform linux/amd64 -t nys-wetlands-dl .
```

### Transferring to HPC

Save the image as a tarball and transfer it:

```bash
docker save nys-wetlands-dl | gzip > nys-wetlands-dl.tar.gz
scp nys-wetlands-dl.tar.gz user@hpc_host:/workdir/user/
```

On the HPC, load the image:

```bash
docker1 load -i /workdir/user/nys-wetlands-dl.tar.gz
```

### Running on HPC

```bash
docker1 run --shm-size=8g --gpus all \
  --user $(id -u):$(id -g) \
  -v /workdir/user/NYS_Wetlands_DL/Data:/app/Data \
  -v /workdir/user/NYS_Wetlands_DL/Models:/app/Models \
  nys-wetlands-dl
```

- **`--user $(id -u):$(id -g)`**: Required — runs the container as your HPC user so output files (checkpoints, logs) are owned by you, not root.
- **`--shm-size=8g`**: Required — PyTorch DataLoader workers use shared memory for IPC. Docker's default (64MB) causes `bus error` crashes.
- **`--gpus all`**: Exposes all available NVIDIA GPUs. Lightning auto-detects multi-GPU and uses DDP.
- **Volume mounts**: `Data/` and `Models/` are mounted at runtime (not baked into the image) so training data and checkpoints persist on the host.

The container runs `Shell_Scripts/DL_model_pipeline_HPC.sh` by default (set in the Dockerfile `CMD`).

### Shell Script Configurations

Two pipeline scripts are provided:

| Parameter | Local (`DL_model_pipeline.sh`) | HPC (`DL_model_pipeline_HPC.sh`) |
|-----------|-------------------------------|----------------------------------|
| BASE_FILTERS | 64 | 128 |
| DEPTH | 4 | 5 |
| EPOCHS | 50 | 100 |
| ASPP_RATES | 6 12 18 | 3 6 12 (for depth=5) |
| KFOLD | 0 (disabled) | 2 (enabled) |

### Monitoring Training

- **CSV logs**: Written to `Models/lightning_logs/<run_name>/metrics.csv` — tail from the HPC host.
- **TensorBoard**: Logs written to `Models/tb_logs/`. View via SSH tunnel:
  ```bash
  ssh -L 6006:localhost:6006 user@hpc_host
  tensorboard --logdir /workdir/user/NYS_Wetlands_DL/Models/tb_logs --port 6006
  ```
  Then open `http://localhost:6006` locally.

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "No band descriptions found" | Ensure GeoTIFF band descriptions are set (use `rasterio` to verify) |
| DataLoader crashes on macOS | Set `--workers 0` (or `NUM_WORKERS=0` in notebook) |
| Out of memory during training | Reduce `--batch-size` or `--base-filters` |
| NaN values in loss | Verify `ignore_index=255` is set; check for corrupted patches. HybridLoss handles all-ignored batches gracefully. |
| Band mismatch during prediction | Input raster band descriptions must match the names in `normalization_stats.json` |
| Bus error in Docker | Add `--shm-size=8g` (or `--ipc=host`) to your `docker run` command — default 64MB shared memory is too small for DataLoader workers |
| Docker build `platform` warning on Mac | Use `docker build --platform linux/amd64` when building for HPC from Apple Silicon |
| Output files owned by root / permission denied | Add `--user $(id -u):$(id -g)` to your `docker run` command so files are written as your HPC user |
| `--base-filters` / `--depth` mismatch | Not an issue for `.ckpt`/`.safetensors` (architecture is auto-detected); only matters for legacy `.pth`, where the flags must match training |
| UNet3+ out of memory | Lower `--batch-size`, use `--precision 16-mixed`, or reduce `--cat-channels`; full-scale skips are heavier than the plain U-Net |

---

## Directory Structure

```
NYS_Wetlands_DL/
├── pyproject.toml                      # Dependencies + uv config
├── uv.lock                            # Lockfile for reproducible installs
├── .venv/                             # Virtual environment (uv-managed)
├── Data/
│   ├── Training_Data/
│   │   ├── R_Patches/                  # GeoTIFF training patches (256x256)
│   │   └── normalization_stats.json    # Generated by Step 1
│   └── Predictions/                    # Output from Step 6
├── Models/
│   ├── best_{mode}_{arch}_*.ckpt      # Best Lightning checkpoint (+ .safetensors/.meta.json)
│   ├── lightning_logs/                 # Training logs (CSV/TensorBoard)
│   ├── SHAP/                           # Per-model SHAP plots + JSON (from Step 7)
│   └── (legacy .pth files)            # From dl_04_train.py if used
└── Python_Code_Analysis/
    └── DL_Pipeline_v2/
        ├── README.md                   # This file
        ├── UNet_Architecture_Overview.md
        ├── dl_band_config.json            # Normalization rules
        ├── dl_band_utils.py               # Shared band utilities
        ├── dl_losses.py                   # FocalLoss + DiceLoss + HybridLoss
        ├── dl_model_utils.py              # Shared model loading (legacy + Lightning)
        ├── dl_01_compute_statistics.py     # Step 1: Stats
        ├── dl_02_dataset.py               # Step 2: Dataset + normalize_bands()
        ├── dl_03_unet_model.py            # Step 3: U-Net architecture
        ├── dl_03_unet3plus_model.py       # Step 3: UNet3+ architecture (--arch unet3plus)
        ├── dl_model_factory.py            # build_net() architecture dispatch
        ├── dl_04_train_lightning.py        # Step 4: Train (Lightning, primary)
        ├── dl_04_train.py                 # Step 4: Train (legacy fallback)
        ├── dl_05_evaluate.py              # Step 5: Evaluate
        ├── dl_05b_evaluate_patches.py     # Step 5b: Per-patch evaluation
        ├── dl_06_predict.py               # Step 6: Predict
        ├── dl_07_shap_analysis.py         # Step 7: SHAP band importance (CLI)
        ├── dl_07_shap_analysis.ipynb      # Step 7: SHAP (interactive)
        └── wetland_pipeline.ipynb         # Interactive notebook
```
