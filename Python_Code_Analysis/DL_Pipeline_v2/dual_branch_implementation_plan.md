# Dual-Branch + Transformer Wetland Segmentation — Claude Code Implementation Plan

## Goal

Implement a multi-branch encoder architecture (`dl_03c_dualbranch.py`) that processes optical/spectral features and terrain/LiDAR features through separate encoder branches with attention-based fusion, following the design pattern from Jamali & Mahdianpari (2022). The new architecture integrates with the existing NYS Wetlands DL Pipeline v2 as a drop-in replacement for the current U-Net, satisfying the same `nn.Module` contract: `(batch, in_channels, H, W)` → `(batch, num_classes, H, W)`.

**Current pipeline state** (from updated README):
- 19-band GeoTIFF patches (18 predictors + 1 label), 256×256 pixels, 245 patches
- 18 input channels: 7 terrain (DEM + 6 derivatives + CHM), 3 spectral indices, 2 SAR, 6 NAIP (r/g/b/nir + 2 derived indices)
- 4 classes: EMW, FSW, SSW, UPL (UPL ~74% of pixels)
- Geomorphon band has been removed (no one-hot expansion needed)
- NAIP raw bands (r, g, b, nir) and derived indices (n_ndvi, n_ndwi) are now included

---

## Architecture Summary

```
Optical Branch (NAIP + spectral + SAR: 11ch)    Terrain Branch (DEM derivatives + CHM: 7ch)
    ResNet-34 encoder                              ResNet-18 encoder
         │                                              │
    [stage1 feats] ──► CrossModalFusion ◄── [stage1 feats]
    [stage2 feats] ──► CrossModalFusion ◄── [stage2 feats]
    [stage3 feats] ──► CrossModalFusion ◄── [stage3 feats]
    [stage4 feats] ──► CrossModalFusion ◄── [stage4 feats]
         │                                              │
         └──────────► Concatenate ◄─────────────────────┘
                          │
                 Shared U-Net Decoder
                 (SE attention + skip connections)
                          │
                     Output logits
                  (batch, num_classes, H, W)
```

**Optional transformer branch** (Phase 2): A lightweight Swin Transformer encoder for the DEM/elevation channel, replacing or augmenting the terrain ResNet-18, following Jamali & Mahdianpari's use of Swin for the LiDAR-derived DEM.

---

## Phase 1: Dual-Branch CNN with Gated Fusion

### Task 1.1 — Band Routing Configuration

**File to modify**: `dl_band_config.json`

Add a `branch_assignment` field that maps each predictor band to a branch. This keeps the band routing declarative and editable without touching model code.

```json
{
  "branch_assignment": {
    "optical": ["EVI", "NDYI", "GDVI", "VV", "VH",
                "r", "g", "b", "nir", "n_ndvi", "n_ndwi"],
    "terrain": ["DEM", "meanc_local", "planc_local", "profc_local",
                "dmv_local", "slope_local", "TPI_local", "CHM"]
  }
}
```

This gives 11 optical channels and 7 terrain channels (18 total, matching current `in_channels`).

**Implementation notes:**
- `dl_band_utils.py` needs a new function `get_branch_channels(stats, config)` that returns the channel count for each branch. Since Geomorph has been removed, there is no one-hot expansion — channel counts map 1:1 to band counts.
- `dl_02_dataset.py` does NOT need to change — it still returns a single `(in_channels, H, W)` tensor. The model itself splits channels by index at forward time.
- `normalization_stats.json` already stores `predictor_names` in order. The model uses `branch_assignment` + `predictor_names` to compute a mapping of channel indices → branch at init time.

### Task 1.2 — Dual-Branch Encoder

**New file**: `dl_03c_dualbranch.py`

Build two separate ResNet-based encoders, one per branch. The optical branch uses a ResNet-34 (deeper, since it handles 11 channels of spectral, NAIP, and SAR features with more complex inter-band relationships). The terrain branch uses a ResNet-18 (shallower — 7 channels of DEM derivatives and CHM with more structured spatial patterns).

```python
class DualBranchEncoder(nn.Module):
    def __init__(self, optical_channels, terrain_channels):
        super().__init__()
        # Optical: ResNet-34 backbone, replace first conv
        self.optical_enc = timm.create_model(
            'resnet34', pretrained=False, in_chans=optical_channels,
            features_only=True, out_indices=(0, 1, 2, 3, 4)
        )
        # Terrain: ResNet-18 backbone, replace first conv
        self.terrain_enc = timm.create_model(
            'resnet18', pretrained=False, in_chans=terrain_channels,
            features_only=True, out_indices=(0, 1, 2, 3, 4)
        )
```

**Key decisions:**
- Use `timm` library for encoder backbones (`features_only=True` returns multi-scale feature maps). `timm` is already compatible with the project's PyTorch/Lightning stack.
- `pretrained=False` since input channels differ from ImageNet's 3 RGB channels. However, with NAIP r/g/b now in the optical branch, a future enhancement could load ImageNet-pretrained weights for the first 3 channels and randomly initialize the remaining 8 — this is a well-established transfer learning pattern in remote sensing.
- Both encoders output feature maps at 5 scales: stride 2, 4, 8, 16, 32.

### Task 1.3 — Cross-Modal Fusion Module

Implement a channel-attention gated fusion module that learns to weight features from each branch at each encoder stage. This replaces naive concatenation with an adaptive mechanism.

```python
class CrossModalFusion(nn.Module):
    """
    Gated fusion: learns per-channel weights for each modality.
    Applied at each encoder stage before passing to the decoder.
    ResNet-18 and ResNet-34 produce identical channel widths per stage
    (64, 64, 128, 256, 512), so no projection is needed.
    """
    def __init__(self, channels):
        super().__init__()
        self.gate = nn.Sequential(
            nn.AdaptiveAvgPool2d(1),
            nn.Flatten(),
            nn.Linear(channels * 2, channels),
            nn.ReLU(inplace=True),
            nn.Linear(channels, channels),
            nn.Sigmoid()
        )

    def forward(self, optical_feat, terrain_feat):
        combined = torch.cat([optical_feat, terrain_feat], dim=1)
        gate_weights = self.gate(combined).unsqueeze(-1).unsqueeze(-1)
        fused = optical_feat * gate_weights + terrain_feat * (1 - gate_weights)
        return fused
```

**Design rationale:** Although ResNet-18 and ResNet-34 produce the same channel widths at each stage (64→128→256→512), using separate encoders allows each branch to learn modality-specific convolutional filters — optical filters can specialize in spectral/textural patterns while terrain filters learn gradient and curvature features. The squeeze-excitation-style gate learns a soft per-channel routing that adapts spatially (e.g., weighting terrain features more heavily in flat, ambiguous areas where spectral cues alone are insufficient).

### Task 1.4 — Shared Decoder

Reuse the existing SE-attention U-Net decoder from `dl_03_unet_model.py`. The fused features from each encoder stage become the skip connections.

```python
class DualBranchUNet(nn.Module):
    def __init__(self, in_channels, num_classes,
                 optical_indices, terrain_indices,
                 base_filters=64, depth=4):
        # ... encoder + fusion setup from above ...
        # Decoder mirrors the existing U-Net decoder structure
        self.decoder = UNetDecoder(
            encoder_channels=fused_channel_list,
            num_classes=num_classes
        )

    def forward(self, x):
        # Split input tensor into branches by channel index
        x_optical = x[:, self.optical_idx, :, :]
        x_terrain = x[:, self.terrain_idx, :, :]

        # Encode each branch
        opt_features = self.optical_enc(x_optical)
        ter_features = self.terrain_enc(x_terrain)

        # Fuse at each scale
        fused = [self.fusions[i](opt_features[i], ter_features[i])
                 for i in range(self.depth)]

        # Decode
        return self.decoder(fused)
```

**Critical**: The model accepts the full `(batch, in_channels, H, W)` tensor and internally splits it using precomputed index arrays. This means `dl_02_dataset.py`, `dl_04_train_lightning.py`, `dl_05_evaluate.py`, and `dl_06_predict.py` require NO changes. The `WetlandSegmentationModule` wraps this model identically to the existing U-Net.

### Task 1.5 — Integration with Training Pipeline

**File to modify**: `dl_04_train_lightning.py`

Add `"dualbranch"` as a new `--architecture` option. The train function constructs the model by reading `branch_assignment` from config and computing channel indices.

```python
elif args.architecture == "dualbranch":
    optical_idx, terrain_idx = get_branch_indices(stats, config)
    net = DualBranchUNet(
        in_channels=in_channels,
        num_classes=num_classes,
        optical_indices=optical_idx,
        terrain_indices=terrain_idx,
        base_filters=args.base_filters,
        depth=args.depth
    )
```

**Downstream scripts** (`dl_05_evaluate.py`, `dl_06_predict.py`) need the same architecture dispatch logic, which can be centralized in `dl_model_utils.py`.

### Task 1.6 — Dependency Update

**File to modify**: `pyproject.toml`

Add `timm` to dependencies:

```toml
dependencies = [
    # ... existing deps ...
    "timm>=1.0.0",
]
```

Then: `uv sync`

---

## Phase 2: Optional Swin Transformer Terrain Branch

Replace the ResNet-18 terrain encoder with a Swin Transformer, following Jamali & Mahdianpari (2022) who used a Swin Transformer specifically for the LiDAR-derived DEM branch.

### Task 2.1 — Swin Terrain Encoder

```python
self.terrain_enc = timm.create_model(
    'swinv2_tiny_window8_256',
    pretrained=False,
    in_chans=terrain_channels,
    features_only=True,
    out_indices=(0, 1, 2, 3)
)
```

**Key considerations:**
- Swin outputs 4 stages (not 5 like ResNet). The fusion module and decoder need to handle the mismatch — either add a stem-level passthrough or adjust fusion to start at stage 1.
- Swin window size (8) must divide the patch size. Current patches are 256×256, which works with window=8.
- Swin is more memory-intensive than ResNet-18. For local development (Apple M1, 32GB), use `swinv2_tiny`. For HPC, `swinv2_small` or `swinv2_base` are viable.
- The transformer branch captures long-range spatial context in terrain features (landscape position, drainage patterns) that the CNN terrain branch cannot model. This is the primary motivation for adding it.

### Task 2.2 — Three-Branch Variant

Full Jamali & Mahdianpari replication: optical CNN + SAR CNN + Swin Transformer for DEM. This requires splitting the current `optical` branch into three sub-groups: NAIP imagery (r, g, b, nir, n_ndvi, n_ndwi), spectral indices (EVI, NDYI, GDVI), and SAR (VV, VH). Defer this to Phase 2 unless initial dual-branch results plateau.

---

## Phase 3: Ablation and Evaluation

### Task 3.1 — Controlled Comparison

Train and evaluate three configurations on the same data splits (same `--seed 42`):

| Configuration | Architecture flag | Description |
|--------------|------------------|-------------|
| Baseline | `--architecture unet` | Current single-encoder U-Net (all 18 channels stacked) |
| Dual-Branch CNN | `--architecture dualbranch` | ResNet-34 optical (11ch) + ResNet-18 terrain (7ch) + gated fusion |
| Dual-Branch + Swin | `--architecture dualbranch_swin` | ResNet-34 optical (11ch) + Swin-Tiny terrain (7ch) + gated fusion |

### Task 3.2 — Metrics to Compare

Use `dl_05_evaluate.py` output for each model:
- Per-class IoU (especially EMW, FSW, SSW — the minority wetland classes)
- Mean IoU
- Per-class F1
- Confusion matrices — look for reduction in wetland-type confusion (e.g., FSW↔SSW)

### Task 3.3 — Feature Attribution (Optional)

Use existing `dl_07_shap_analysis.ipynb` to compare which branches/bands drive predictions. Expect terrain features (TPI, slope, curvatures, CHM) to contribute more in the dual-branch model than in the single-encoder baseline, since they get dedicated learned representations.

---

## File Change Summary

| File | Action | Description |
|------|--------|-------------|
| `dl_band_config.json` | Modify | Add `branch_assignment` field |
| `dl_band_utils.py` | Modify | Add `get_branch_channels()` and `get_branch_indices()` |
| `dl_03c_dualbranch.py` | **New** | Dual-branch encoder + fusion + decoder |
| `dl_04_train_lightning.py` | Modify | Add `dualbranch` architecture option |
| `dl_model_utils.py` | Modify | Add architecture dispatch for checkpoint loading |
| `dl_05_evaluate.py` | Modify | Add architecture dispatch (minor) |
| `dl_06_predict.py` | Modify | Add architecture dispatch (minor) |
| `pyproject.toml` | Modify | Add `timm` dependency |
| `README.md` | Modify | Document new architecture in Swappable Architectures table |

---

## Implementation Order for Claude Code

Execute these steps sequentially. Each step should be a self-contained, testable unit.

```
Step 1: Add timm to pyproject.toml and uv sync
Step 2: Add branch_assignment to dl_band_config.json
Step 3: Add get_branch_channels() and get_branch_indices() to dl_band_utils.py
Step 4: Create dl_03c_dualbranch.py — CrossModalFusion module only (test standalone)
Step 5: Create dl_03c_dualbranch.py — DualBranchUNet class with forward pass
Step 6: Add smoke test at bottom of dl_03c_dualbranch.py (dummy tensor forward pass)
Step 7: Add "dualbranch" dispatch to dl_04_train_lightning.py
Step 8: Add "dualbranch" dispatch to dl_model_utils.py, dl_05_evaluate.py, dl_06_predict.py
Step 9: Update README.md Swappable Architectures table
Step 10 (Phase 2): Add Swin Transformer terrain encoder variant
```

---

## References

- Jamali, A. & Mahdianpari, M. (2022). Swin Transformer and Deep Convolutional Neural Networks for Coastal Wetland Classification Using Sentinel-1, Sentinel-2, and LiDAR Data. *Remote Sensing*, 14(2), 359.
- Effah, D. et al. (2025). Advances in Machine Learning for Wetland Classification. *Artificial Intelligence Review*.
- Maxwell, A. et al. (2023). Exploring the Influence of Input Feature Space on CNN Wetland Mapping. *Earth and Space Science*.
- Mainali, K. et al. (2023). CNN for High-Resolution Wetland Mapping with Open Data. *Science of the Total Environment*.
- MMFNet (2025). Multimodal Fusion Framework for RS Semantic Segmentation. *Neurocomputing*.
- SGFN (2024). Semantic Guidance Fusion Network for Cross-Modal Segmentation. *Sensors*.
- Zhou, A. et al. (2023). Multi-Terrain Feature-Based Deep CNN for Super-Resolution DEMs. *Int. J. Applied Earth Obs. Geoinf.*
