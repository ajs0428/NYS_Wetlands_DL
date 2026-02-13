# U-Net Model Architecture Overview

## Summary

| Aspect | Value |
|--------|-------|
| **Architecture** | U-Net with residual encoder blocks and SE-attention decoder blocks |
| **Depth** | 4 (configurable; 5 for HPC) |
| **Base Filters** | 32 (doubles each level: 32→64→128→256→512) |
| **Input Channels** | 29 (20 predictor bands, with geomorphology one-hot expanded to 10) |
| **Output Classes** | 5 (EMW, FSW, OWW, SSW, UPL) |
| **Total Parameters** | ~7.8M (local) / ~125.3M (HPC) |
| **Patch Size** | 128×128 pixels |
| **Activation** | ReLU (inplace) |
| **Normalization** | BatchNorm after every convolution (except final output) |
| **Loss Function** | Hybrid CrossEntropy + Dice |
| **Dropout** | None |

---

## Building Blocks

### ConvBlock

The fundamental unit — a double convolution block with optional residual connection:

```
                    ┌──────────────────────────────────┐
                    │          shortcut path            │
                    │  (Identity if channels match,     │
                    │   1×1 Conv+BN if they differ)     │
                    │                                   │
Input ──────────────┤                                   (+) → ReLU → Output
                    │                                   │
                    └→ Conv2d(3×3) → BN → ReLU          │
                              ↓                         │
                       Conv2d(3×3) → BN ────────────────┘
```

- **`residual=False`** (default): standard sequential Conv-BN-ReLU × 2 (used in bottleneck and decoder)
- **`residual=True`**: adds a shortcut connection from input to output. The final ReLU is applied *after* the addition (standard ResNet pattern). When `in_channels != out_channels`, a 1×1 Conv+BN projection aligns dimensions.

Bias is omitted in convolutions because BatchNorm provides its own learnable shift parameter.

### EncoderBlock

Downsampling block with residual convolutions that captures a skip connection:

```
Input → ConvBlock(residual=True) → skip_output ──────→ (saved for decoder)
                                       ↓
                                  MaxPool2d(2×2) → pooled_output
```

The residual connection inside ConvBlock allows gradients to flow directly through the encoder, which is especially helpful with 29 heterogeneous input channels where the first encoder block (29→32) must learn very different transformations per band.

### SqueezeExcitation (SE)

Channel attention module that learns to reweight feature channels:

```
Input (B, C, H, W) → AdaptiveAvgPool → FC(C → C/r) → ReLU → FC(C/r → C) → Sigmoid → scale
                                                                                         ↓
Output = Input × scale
```

- **Reduction ratio**: r=16, with a floor of 8 neurons (so the 32-channel decoder level uses 8 instead of 2)
- Placed after the ConvBlock in each decoder block, operating on the fused encoder+decoder representation
- Learns which channels are most informative at each resolution level

### DecoderBlock

Upsampling block that fuses encoder features via skip connections, followed by SE channel attention:

```
Input → ConvTranspose2d(2×2) → [pad if size mismatch] → Concatenate(skip) → ConvBlock → SE → output
```

Handles odd-dimension mismatches with asymmetric `F.pad` to ensure skip connection tensors align correctly. The SE block after the ConvBlock adaptively reweights channels based on the fused skip+upsampled features.

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

A 1×1 convolution maps the final 32 feature channels to 5 class logits. No softmax is applied — the CE component of `HybridLoss` expects raw logits; the Dice component applies softmax internally.

**Output shape:** `(batch_size, 5, 128, 128)` — per-pixel class logits.

---

## Diagram

```
Input (29, 128, 128)
  │
  ├─ ResEncoder L0 ── skip₀ (32, 128, 128) ────────────────────────────┐
  │  ↓ pool                                                             │
  ├─ ResEncoder L1 ── skip₁ (64, 64, 64) ───────────────────┐          │
  │  ↓ pool                                                  │          │
  ├─ ResEncoder L2 ── skip₂ (128, 32, 32) ────────┐         │          │
  │  ↓ pool                                        │         │          │
  ├─ ResEncoder L3 ── skip₃ (256, 16, 16) ──┐     │         │          │
  │  ↓ pool                                  │     │         │          │
  │                                          │     │         │          │
  └─ Bottleneck (512, 8, 8)                  │     │         │          │
          │                                  │     │         │          │
          ├─ Decoder+SE L3 ← concat(skip₃) ─┘     │         │          │
          │  (256, 16, 16)                         │         │          │
          ├─ Decoder+SE L2 ← concat(skip₂) ───────┘         │          │
          │  (128, 32, 32)                                   │          │
          ├─ Decoder+SE L1 ← concat(skip₁) ─────────────────┘          │
          │  (64, 64, 64)                                               │
          ├─ Decoder+SE L0 ← concat(skip₀) ────────────────────────────┘
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

Unlabeled pixels are mapped to index **255** and excluded from both loss components — CE via `ignore_index=255`, Dice via explicit masking before computing per-class overlap.

---

## Training Configuration

| Parameter | Value |
|-----------|-------|
| **Loss** | HybridLoss = CrossEntropy + Dice (equal weight) |
| **CE component** | Class-weighted (inverse frequency), ignore_index=255 |
| **Dice component** | Per-class softmax Dice, averaged across classes (inherently balanced), smooth=1.0 |
| **Optimizer** | AdamW (lr=1e-4, weight_decay=1e-4) |
| **LR Scheduler** | ReduceLROnPlateau (factor=0.5, patience=5) |
| **Class Weighting** | Inverse frequency on CE; Dice is inherently class-balanced |
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
| Approx. parameters | ~7.8M | ~125.3M |

---

## Key Design Decisions

1. **Residual encoder blocks** — shortcut connections in each encoder level improve gradient flow through the network. A 1×1 projection aligns channel dimensions when they differ (e.g., 29→32 at the first level). The residual path preserves raw input signal while the conv path learns transformations.
2. **Squeeze-and-Excitation in decoder** — channel attention after fusing skip connections lets the model learn which feature channels are most relevant at each resolution level. Particularly valuable with heterogeneous inputs (spectral, terrain, SAR, categorical).
3. **Hybrid CE + Dice loss** — CrossEntropy provides stable per-pixel gradients (with class weights for imbalance); Dice directly optimizes region overlap (closer to the mIoU evaluation metric). Equal weighting; CE handles `ignore_index=255` natively, Dice masks ignored pixels explicitly.
4. **No dropout** — relies on BatchNorm for regularization combined with early stopping on validation loss.
5. **Transposed convolutions** for upsampling (not bilinear interpolation) — learnable upsampling parameters.
6. **Skip connection via concatenation** (not addition) — preserves full information from both encoder and decoder paths, at the cost of higher channel counts before each decoder ConvBlock.
7. **No softmax in forward pass** — CE applies log-softmax internally; Dice applies softmax internally.
8. **Dynamic input channels** — `in_channels` is computed at runtime from band configuration, not hardcoded. Model checkpoints store `in_channels` and `num_classes` for correct reloading.
