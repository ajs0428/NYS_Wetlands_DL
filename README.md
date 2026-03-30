# NYS Wetlands Deep Learning

A deep learning pipeline for wetland semantic segmentation in New York State. The project uses a U-Net encoder-decoder architecture with residual blocks and squeeze-and-excitation (SE) attention, trained on multi-source remote sensing data (terrain, spectral indices, SAR, and NAIP imagery) to classify pixels into wetland categories. Training data is derived from the National Wetlands Inventory and organized by HUC12 watersheds for systematic processing across the state.

The pipeline supports two classification modes, toggled via `dl_band_config.json`:

- **Multiclass** (default): Four classes — EMW, FSW, SSW, UPL
- **Binary**: Two classes — WET (all wetland types merged) vs UPL (upland)

Binary mode remaps labels at runtime so both modes use the same training patches. See the [pipeline README](Python_Code_Analysis/DL_Pipeline_v2/README.md) for details.

Training patches are currently 256x256 pixels, but the pipeline is patch-size agnostic — the U-Net is fully convolutional and all dimensions are discovered at runtime. To use different patch sizes, just create patches at the desired size, set `PATCH_SIZE` accordingly for prediction, and retrain. The only constraint is that the patch size must be divisible by 2^depth (e.g., 16 for depth=4, 32 for depth=5).

## Environment Setup

**uv (recommended):**

```bash
uv sync                        # core dependencies
uv sync --extra notebooks      # include Jupyter and SHAP
source .venv/bin/activate
```

For CUDA-enabled PyTorch on HPC, add the appropriate index:

```bash
uv sync --extra-index-url https://download.pytorch.org/whl/cu121
```

**Conda** (alternative):

```bash
conda env create -f Python_Code_Analysis/wetland-cnn-env.yml
conda activate wetland-cnn
```

Dependencies are defined in `pyproject.toml`.

## Docker (BioHPC Deployment)

The project includes a Dockerfile for running on Cornell BioHPC GPU nodes. The image uses `pytorch/pytorch` with CUDA 11.8, compatible with all available GPU nodes (A40, A100, H100, A6000).

### 1. Transfer code to HPC

```bash
rsync -av --exclude='.venv' --exclude='Data' --exclude='Models' --exclude='.git' \
  "/path/to/NYS_Wetlands_DL/" \
  <username>@<gpu-node>.biohpc.cornell.edu:/workdir/<labid>/nys_wetlands/
```

Or `git clone` directly on the HPC (Data/ and Models/ are gitignored).

### 2. Copy data from network storage

```bash
mkdir -p /workdir/<labid>/nys_wetlands/Data/Training_Data
cp -r /network/mount/path/R_Patches /workdir/<labid>/nys_wetlands/Data/Training_Data/
cp /network/mount/path/normalization_stats.json /workdir/<labid>/nys_wetlands/Data/Training_Data/

mkdir -p /workdir/<labid>/nys_wetlands/Models
```

### 3. Build the Docker image

```bash
cd /workdir/<labid>/nys_wetlands
docker1 build -t biohpc_<labid>/wetland-dl .
```

### 4. Run training

```bash
docker1 run --gpus all \
  -v /workdir/<labid>/nys_wetlands/Data:/app/Data \
  -v /workdir/<labid>/nys_wetlands/Models:/app/Models \
  biohpc_<labid>/wetland-dl
```

### Other run modes

```bash
# Interactive shell
docker1 run --gpus all -it \
  -v /workdir/<labid>/nys_wetlands/Data:/app/Data \
  -v /workdir/<labid>/nys_wetlands/Models:/app/Models \
  biohpc_<labid>/wetland-dl /bin/bash

# Run a specific script
docker1 run --gpus all \
  -v /workdir/<labid>/nys_wetlands/Data:/app/Data \
  -v /workdir/<labid>/nys_wetlands/Models:/app/Models \
  biohpc_<labid>/wetland-dl \
  python Python_Code_Analysis/DL_Pipeline_v2/dl_05_evaluate.py

# Mount code for live editing (no rebuild needed)
docker1 run --gpus all -it \
  -v /workdir/<labid>/nys_wetlands/Data:/app/Data \
  -v /workdir/<labid>/nys_wetlands/Models:/app/Models \
  -v /workdir/<labid>/nys_wetlands/Python_Code_Analysis:/app/Python_Code_Analysis \
  biohpc_<labid>/wetland-dl /bin/bash
```

### Jupyter + TensorBoard

BioHPC restricts web services to ports 8009–8039. Use `find_open_ports` on the HPC to check availability.

```bash
# Start container with ports in the allowed range (e.g., 8015 for TensorBoard, 8016 for Jupyter)
docker1 run --gpus all -it \
  -p 8015:8015 -p 8016:8016 \
  -v /workdir/<labid>/nys_wetlands/Data:/app/Data \
  -v /workdir/<labid>/nys_wetlands/Models:/app/Models \
  -v /workdir/<labid>/nys_wetlands/Python_Code_Analysis:/app/Python_Code_Analysis \
  biohpc_<labid>/wetland-dl /bin/bash

# Inside the container:
tensorboard --logdir Models/ --bind_all --port 8015 &
jupyter lab --ip=0.0.0.0 --port=8016 --no-browser --allow-root
```

From the Cornell network (or VPN), open in your browser:
- **Jupyter:** `http://<gpu-node>.biohpc.cornell.edu:8016`
- **TensorBoard:** `http://<gpu-node>.biohpc.cornell.edu:8015`

Note: BioHPC uses `docker1` instead of `docker`. Name images `biohpc_<labid>/...` to prevent automated cleanup. See [BioHPC Docker guide](https://biohpc.cornell.edu/Lab/userguide.aspx?a=software&i=340#c) for details.
