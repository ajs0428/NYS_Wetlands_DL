# U-Net Model Architecture Overview

## Summary

| Aspect | Value |
|--------|-------|
| **Architecture** | U-Net with 4 encoder/decoder levels |
| **Depth** | 4 (configurable; 5 for HPC) |
| **Base Filters** | 32 (doubles each level: 32→64→128→256→512) |
| **Input Channels** | 29 (20 predictor bands, with geomorphology one-hot expanded to 10) |
| **Output Classes** | 5 (EMW, FSW, OWW, SSW, UPL) |
| **Total Parameters** | ~7.1M |
| **Patch Size** | 128×128 pixels |
| **Activation** | ReLU (inplace) |
| **Normalization** | BatchNorm after every convolution (except final output) |
| **Dropout** | None |

---

## Building Blocks

### ConvBlock

The fundamental unit — a double convolution block:

```
Conv2d(3×3, padding=1, no bias) → BatchNorm2d → ReLU
                ↓
Conv2d(3×3, padding=1, no bias) → BatchNorm2d → ReLU
```

Bias is omitted in convolutions because BatchNorm provides its own learnable shift parameter.

### EncoderBlock

Downsampling block that captures a skip connection:

```
Input → ConvBlock → skip_output ──────→ (saved for decoder)
                        ↓
                   MaxPool2d(2×2) → pooled_output
```

### DecoderBlock

Upsampling block that fuses encoder features via skip connections:

```
Input → ConvTranspose2d(2×2) → [pad if size mismatch] → Concatenate(skip) → ConvBlock → output
```

Handles odd-dimension mismatches with asymmetric `F.pad` to ensure skip connection tensors align correctly.

---

## Full Architecture

### Filter Progression

```
Level:    L0    L1    L2     L3     Bottleneck
Filters:  32    64    128    256    512
```

### Encoder Path (Downsampling)

| Level | Block | Spatial Size | Channels |
|-------|-------|-------------|----------|
| Input | — | 128×128 | 29 |
| L0 | EncoderBlock | 128→64 | 29→32 |
| L1 | EncoderBlock | 64→32 | 32→64 |
| L2 | EncoderBlock | 32→16 | 64→128 |
| L3 | EncoderBlock | 16→8 | 128→256 |

Skip connections are captured at each level *before* the max-pool operation.

### Bottleneck

| Block | Spatial Size | Channels |
|-------|-------------|----------|
| ConvBlock | 8×8 | 256→512 |

The deepest representation — no pooling, just the double convolution.

### Decoder Path (Upsampling + Skip Connections)

| Level | Block | Spatial Size | Channel Flow |
|-------|-------|-------------|-------------|
| L3 | DecoderBlock | 8→16 | 512 → upsample → concat(256 skip) → 256 |
| L2 | DecoderBlock | 16→32 | 256 → upsample → concat(128 skip) → 128 |
| L1 | DecoderBlock | 32→64 | 128 → upsample → concat(64 skip) → 64 |
| L0 | DecoderBlock | 64→128 | 64 → upsample → concat(32 skip) → 32 |

### Output Layer

```
Conv2d(32 → 5, kernel_size=1)
```

A 1×1 convolution maps the final 32 feature channels to 5 class logits. No softmax is applied — `CrossEntropyLoss` expects raw logits.

**Output shape:** `(batch_size, 5, 128, 128)` — per-pixel class logits.

---

## Diagram

```
Input (29, 128, 128)
  │
  ├─ Encoder L0 ─── skip₀ (32, 128, 128) ──────────────────────────────┐
  │  ↓ pool                                                             │
  ├─ Encoder L1 ─── skip₁ (64, 64, 64) ─────────────────────┐          │
  │  ↓ pool                                                  │          │
  ├─ Encoder L2 ─── skip₂ (128, 32, 32) ──────────┐         │          │
  │  ↓ pool                                        │         │          │
  ├─ Encoder L3 ─── skip₃ (256, 16, 16) ───┐      │         │          │
  │  ↓ pool                                 │      │         │          │
  │                                         │      │         │          │
  └─ Bottleneck (512, 8, 8)                 │      │         │          │
          │                                 │      │         │          │
          ├─ Decoder L3 ← concat(skip₃) ───┘      │         │          │
          │  (256, 16, 16)                         │         │          │
          ├─ Decoder L2 ← concat(skip₂) ──────────┘         │          │
          │  (128, 32, 32)                                   │          │
          ├─ Decoder L1 ← concat(skip₁) ────────────────────┘          │
          │  (64, 64, 64)                                               │
          ├─ Decoder L0 ← concat(skip₀) ───────────────────────────────┘
          │  (32, 128, 128)
          │
          └─ Conv2d 1×1 → Output (5, 128, 128)
```

---

## Input Channels Breakdown

The 29 input channels come from 20 predictor bands, with the categorical geomorphology band expanded via one-hot encoding:

| Band Type | Count | Normalization |
|-----------|-------|---------------|
| NAIP (R, G, B, NIR) | 4 | divide by 255 |
| NDVI, NDWI | 2 | shift+scale |
| DEM, CHM, Slope, TPI | 4 | min-max |
| Other spectral/terrain bands | 9 | min-max |
| Geomorphology (one-hot, 10 classes) | 10 | categorical |
| **Total** | **29** | |

Band names and normalization methods are not hardcoded — they are discovered from rasterio band descriptions at runtime and configured via `band_config.json`.

---

## Output Classes

| Index | Code | Description |
|-------|------|-------------|
| 0 | EMW | Emergent Wetland |
| 1 | FSW | Forested Wetland |
| 2 | OWW | Open Water Wetland |
| 3 | SSW | Scrub-Shrub Wetland |
| 4 | UPL | Upland (non-wetland) |

Unlabeled pixels are mapped to index **255** and excluded from the loss function via `CrossEntropyLoss(ignore_index=255)`.

---

## Training Configuration

| Parameter | Value |
|-----------|-------|
| **Loss** | CrossEntropyLoss (class-weighted, ignore_index=255) |
| **Optimizer** | AdamW (lr=1e-4, weight_decay=1e-4) |
| **LR Scheduler** | ReduceLROnPlateau (factor=0.5, patience=5) |
| **Class Weighting** | Inverse frequency, normalized so min non-zero weight = 1.0 |
| **Data Augmentation** | Random horizontal/vertical flips + 90° rotations (train only) |
| **Checkpointing** | Best model saved when validation loss improves |
| **Metrics** | Pixel accuracy, per-class IoU, mean IoU |

---

## HPC Configuration

For larger-scale runs, `create_model()` supports an HPC profile:

| Setting | Local | HPC |
|---------|-------|-----|
| base_filters | 32 | 64 |
| depth | 4 | 5 |
| Filter progression | 32→64→128→256→512 | 64→128→256→512→1024→2048 |
| Approx. parameters | ~7.1M | ~113M |

---

## Key Design Decisions

1. **No dropout** — relies on BatchNorm for regularization combined with early stopping on validation loss.
2. **Transposed convolutions** for upsampling (not bilinear interpolation) — learnable upsampling parameters.
3. **Skip connection via concatenation** (not addition) — preserves full information from both encoder and decoder paths, at the cost of higher channel counts before each decoder ConvBlock.
4. **No softmax in forward pass** — `CrossEntropyLoss` applies log-softmax internally for numerical stability.
5. **Dynamic input channels** — `in_channels` is computed at runtime from band configuration, not hardcoded. Model checkpoints store `in_channels` and `num_classes` for correct reloading.
