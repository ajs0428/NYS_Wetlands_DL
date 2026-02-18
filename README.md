# NYS Wetlands Deep Learning

A deep learning pipeline for wetland semantic segmentation in New York State. The project uses a U-Net architecture trained on multi-source remote sensing data (terrain, spectral indices, SAR, and NAIP imagery) to classify pixels into wetland categories. Training data is derived from the National Wetlands Inventory and organized by HUC12 watersheds for systematic processing across the state.

The pipeline supports two classification modes, toggled via `band_config.json`:

- **Multiclass** (default): Five classes — EMW, FSW, OWW, SSW, UPL
- **Binary**: Two classes — WET (all wetland types merged) vs UPL (upland)

Binary mode remaps labels at runtime so both modes use the same training patches. See the [pipeline README](Python_Code_Analysis/DL_Pipeline_v2/README.md) for details.

Training patches are currently 128x128 pixels, but the pipeline is patch-size agnostic — the U-Net is fully convolutional and all dimensions are discovered at runtime. To use larger patches (e.g., 256x256), just create patches at the desired size, set `PATCH_SIZE` accordingly for prediction, and retrain. The only constraint is that the patch size must be divisible by 2^depth (e.g., 16 for depth=4, 32 for depth=5).
