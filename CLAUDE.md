# NYS Wetlands Deep Learning

## Project Overview
U-Net semantic segmentation pipeline for wetland classification in New York State. Multi-source remote sensing inputs (terrain, spectral, SAR, NAIP imagery) -> pixel-level wetland type predictions.

## Environment
- **Package manager:** uv (preferred) or conda
- **Python:** 3.11
- **Key libraries:** PyTorch, Lightning, rasterio, scikit-learn, geopandas, matplotlib
- **Activate (uv):** `source .venv/bin/activate`
- **Activate (conda):** `conda activate wetland-cnn`
- **Install/sync:** `uv sync` (Mac/CPU) or `uv sync --extra-index-url https://download.pytorch.org/whl/cu121` (HPC/CUDA)

## Project Structure
```
Python_Code_Analysis/DL_Pipeline_v2/   # Main pipeline (production)
  dl_01_compute_statistics.py             # Scan patches -> normalization_stats.json
  dl_02_dataset.py                        # PyTorch Dataset, normalization, splits
  dl_03_unet_model.py                     # U-Net with residual blocks + SE attention
  dl_03_unet3plus_model.py                # UNet3+ (full-scale skips + deep supervision; --arch unet3plus)
  dl_model_factory.py                     # build_net() architecture dispatch (unet | unet3plus)
  dl_04_train.py                          # Legacy training loop (fallback/reference)
  dl_04_train_lightning.py                # Lightning training (primary)
  dl_05_evaluate.py                       # Test metrics, confusion matrix, IoU
  dl_05b_evaluate_patches.py              # Per-patch evaluation
  dl_06_predict.py                        # Sliding-window inference -> GeoTIFF
  dl_07_shap_analysis.ipynb               # Feature importance
  dl_losses.py                            # FocalLoss + DiceLoss + HybridLoss
  dl_model_utils.py                       # Shared model loading (legacy + Lightning checkpoints)
  dl_band_utils.py                        # Shared band discovery/config utilities
  dl_band_config.json                     # Normalization rules, class names, mode
  sweep.py                                # Loss hyperparameter sweep
  wetland_pipeline.ipynb                  # Interactive notebook (full pipeline)
  dl_experiment_config.py                 # Factorial: 8-config matrix (single source of truth)
  dl_08_aggregate_factorial.py            # Factorial: Phase-3 aggregation (CPU/pandas)
  dl_08b_aggregate_patchcurve.py          # Follow-on: patch-curve + --arch-compare aggregation (CPU)
  dl_09_shap_factorial.py                 # Factorial: per-cell SHAP (GPU, in-container)
  dl_10_factorial_viz.ipynb               # Lightweight figures §1-§7 (CSV/JSON only; git-syncable; wetland-cnn or uv kernel)
  dl_10b_huc_inference_viz.ipynb          # Data-heavy §8: HUC prevalence from prediction GeoTIFFs (rsync rasters separately)
  dl_huc_stack.py                         # Follow-on: build per-HUC inference stack
  factorial_experiment/EXECUTION.md       # Factorial: full operational walkthrough (read this)
  production_model/dl_prod_config.py      # Production: the shipped model's recipe (single source of truth)
  production_model/PLAN.md                # Production: recipe rationale + open decisions
  production_model/EXECUTION.md           # Production: operational walkthrough (deltas from factorial)
Shell_Scripts/                         # Orchestration wrappers (run_factorial.sh, run_*.sh, rsync_*.sh)
Models/production_model/               # Production: the deployable model's cells (+ its checkpoints)
webmap/                                # Leaflet/COG viewer + dev server (source tracked; COGs are not)
Models/factorial_results/              # Synced base-factorial cells + analysis/
Models/results_patchcurve/             # Follow-on: learning-curve cells + analysis/
Models/results_arch/                   # Follow-on: UNet3+ cells + analysis/
Data/HUC_DL_Predictions/               # Follow-on: HUC inference GeoTIFFs (class + probs)
Models/                                # Trained checkpoints + evaluation outputs
Data/Training_Data/R_Patches/          # 256x256 GeoTIFF training patches
Data/Training_Data/normalization_stats.json
pyproject.toml                         # Dependencies + uv config
```

## Repo & Branching
- **`main` is the trunk.** All work lands here. (History note: `factorial-experiment-pipeline` had drifted into being the de-facto trunk while `main` sat abandoned; merged back into `main` 2026-07-27.)
- **Short-lived feature branches only** — days, not months — for changes risky enough to want isolation (a `dl_02_dataset.py` refactor, a new architecture). Name them `feat/<thing>` / `fix/<thing>` and merge back fast.
- **Never branch to separate a workstream.** Workstreams here share the entire core (`dl_01`–`dl_06`, dataset, model, losses), so a long-lived parallel branch means every core fix is applied twice and reconciled at merge — and this repo's merges are `.ipynb`-JSON and `.gitignore` merges, the painful kind.
- **Separate workstreams by directory + results root + config instead**, keeping the 1:1:1 mapping: `<workstream>` = `Shell_Scripts/run_<workstream>.sh` = `Models/<workstream>/`, with a `<workstream>/` doc+config dir under `DL_Pipeline_v2/`. Current instances: `factorial_experiment` → `Models/factorial_results_v2/`; `production_model` → `Models/production_model/`.
- **Freeze a published result with a tag, not a branch** (`factorial-v1`, `factorial-v2`). Tags are immutable and need no maintenance.
- **`.gitignore` starts with a blanket `*`** — anything new is invisible until explicitly whitelisted, and reaching a deep file requires un-ignoring every parent dir first. Check with `git check-ignore -v --no-index <path>` (without `--no-index` it silently skips already-tracked files).

## Pipeline Workflow
Run scripts in order: dl_01 -> dl_02 (imported by dl_04) -> dl_03 (imported by dl_04) -> dl_04_train_lightning -> dl_05 -> dl_06
- **Primary training:** `dl_04_train_lightning.py` (Lightning Trainer with callbacks)
- **Legacy training:** `dl_04_train.py` (manual loop, kept as fallback)
- **Architecture:** selectable via `--arch` — `unet` (default, `dl_03_unet_model.py`, residual blocks + SE attention) or `unet3plus` (`dl_03_unet3plus_model.py`, full-scale skips + deep supervision). Dispatched by `dl_model_factory.build_net()`.

## Key Conventions
- **Band handling is dynamic** — band names/indices are discovered from raster descriptions at runtime, never hardcoded
- **Configuration lives in `dl_band_config.json`** — normalization methods, classification mode, class names, ignore_index
- **Stats live in `normalization_stats.json`** — generated by step 01, consumed by all downstream scripts
- **Use rasterio** for all raster I/O (not GDAL directly)
- **NaN handling:** Always use `np.isnan()`, never `val == nan` (IEEE 754)
- **ignore_index=255** for unlabeled pixels in CrossEntropyLoss
- **Two classification modes:** multiclass (EMW/FSW/SSW/UPL) and binary (WET/UPL), toggled in dl_band_config.json
- **Git safety (HARD RULE):** never run `git checkout -- <file>`, `git restore`, or `git reset --hard` on a file with uncommitted working-tree changes — it discards unstaged work irreversibly (no reflog, no dangling blob). Much of this repo's notebook/analysis work lives **uncommitted** in the working tree (`Models/`, `Data/`, and in-progress notebook cells are gitignored or unstaged). A large diff on a `.ipynb` is almost always real work (output blobs inflate line counts), **not** corruption to undo. If a revert seems needed, `cp` the file to the scratchpad first and ask. (Lesson learned 2026-06-27: a panic `git checkout` destroyed the notebook's uncommitted §6–8 follow-on cells.)

## Architecture Details
- **Selection:** `--arch {unet,unet3plus}` (default `unet`) on train/eval/predict; `dl_model_factory.build_net()` is the single dispatch point. Each arch ignores the other's flags. Architecture + hyperparams are stored in the checkpoint/`.meta.json`/`training_log.json` and auto-detected on load, so eval/predict need no `--arch` for `.ckpt`/`.safetensors`.
- **U-Net:** Residual blocks + SE attention (depth 4 local / 5 HPC, base filters 32/64). Optional ASPP module at bottleneck (`--use-aspp`) expands receptive field to ~250m+ via parallel dilated convolutions (rates 6/12/18 default; use 3/6/12 for depth=5). Off by default for backward compatibility.
- **UNet3+** (`--arch unet3plus`): full-scale skip connections (each decoder node aggregates all encoder scales + deeper decoder nodes + bottleneck, unified to `--cat-channels` width, default 64 -> decoder nodes are `cat_channels*(depth+1)` wide). Optional `--deep-supervision` adds a loss head per decoder stage + bottleneck; the net returns a list of full-res heads in train mode and a single tensor in eval (`_shared_step` handles both). Reuses U-Net's ConvBlock/SE blocks. ~15M params at bf=32/d4 (~2x the plain U-Net); memory-heavy — prefer `16-mixed` / smaller batch on HPC.
- Input: 31 channels (22 predictor bands; Geomorph_local one-hot expands 1 band to 10 channels)
- Loss: Hybrid Focal + Dice with class weights (in `dl_losses.py`)
- Optimizer: AdamW + ReduceLROnPlateau
- Callbacks: ModelCheckpoint, EarlyStopping, LearningRateMonitor
- Patch size must be divisible by 2^depth (16 for depth=4, 32 for depth=5)

## Data Notes
- Training patches: 256x256 pixels, 23 bands (22 predictors + 1 label)
- Patch sources and naming convention
  - Patches are contained in folders that denote their model-use
    - `R_Patches` — Field and GIS-annotated patches (our high quality)
    - `R_Patches_NWI` – Patches from the NWI and are NOT annotated or fixed, so they are direct sources without any augmentation.
  - Patch naming conventions denote data sources, the spatial location, and spatial dimensions.
    - In `R_Patches` the patch `NHP_AJS_cluster_64_huc_041402010202_patch_1_256m.tif`
      - `NHP_` = The source data
      - `AJS_` = The patch annotator 
      - `cluster_64_` = the cluster of HUC12 watersheds 
      - `huc_041402010202_` = the specific HUC12 in the cluster
      - `patch_1_` = the unique patch number 
      - `256m` = the dimensions of the patch
    - In `R_Patches_NWI` the patch `NHP_AJS_cluster_64_huc_041402010202_patch_1_256m.tif`
      - `NHP_` = The source data
      - `AJS_` = The patch annotator 
      - `cluster_64_` = the cluster of HUC12 watersheds 
      - `huc_041402010202_` = the specific HUC12 in the cluster
      - `patch_1_` = the unique patch number 
      - `256m` = the dimensions of the patch
  - Patch data sources so far include 
    - `ADK_WCT`
    - `gps` 
    - `NWI`
    - `NEW`
    - `NHP`
    - `TompkinsCountyWetlands2012`
- **Predictor bands (22):**
  - Terrain (7): DEM, meanc_local, planc_local, profc_local, slope_local, flowacc, twi
  - Vegetation structure (3): CHM, pct_below_0.5m, pct_0.5_to_2m
  - Spectral indices (3): EVI, NDYI, GDVI
  - SAR (2): VV, VH
  - NAIP imagery (4): r, g, b, nir
  - NAIP-derived indices (2): n_ndvi, n_ndwi
  - Geomorphon (1): Geomorph_local (one-hot encoded to 10 channels)
- **Normalization:** min_max bands use global raster statistics (`--global-stats` flag in dl_01) to ensure normalization covers full inference range. Spectral indices (EVI, NDYI, GDVI, n_ndvi, n_ndwi) use shift_scale [-1,1]->[0,1]. Geomorph_local uses one-hot encoding (10 classes).
- **Label band:** MOD_CLASS
- Classes: 0=EMW, 1=FSW, 2=SSW, 3=UPL, 255=unlabeled (OWW removed; can be re-added via dl_band_config.json)
- **Class balance:** UPL=74.4%, FSW=13.3%, EMW=6.6%, SSW=5.7%
- Git tracks only code/scripts (Data/ and Models/ are gitignored)

## Checkpoint Compatibility
- **Three formats supported** (in priority order):
  1. **safetensors** (`.safetensors` + `.meta.json` sidecar) — safe (no pickle), fast, self-describing architecture
  2. **Lightning** (`.ckpt`) — includes `hyper_parameters` for architecture auto-detection
  3. **Legacy** (`.pth`) — requires manual `--base-filters`, `--depth`, etc. flags
- Training auto-exports `.safetensors` alongside `.ckpt`; `load_model()` prefers sibling `.safetensors` when present
- Architecture params (in_channels, base_filters, depth, dropout, use_aspp, aspp_rates) are stored in Lightning checkpoints and `.meta.json` sidecars
- Convert existing `.ckpt` files: `python dl_model_utils.py Models/best_model.ckpt --base-filters 64 --depth 5`
- `dl_model_utils.py` handles all three formats; evaluate and predict scripts work with any format

## Factorial Experiment & Follow-on Studies
A controlled benchmark over an **8-config × 3-seed grid (24 "cells")** isolating the effect of LiDAR tier, leaf-off NAIP, and label source on wetland segmentation, plus three follow-on studies (learning curve, UNet3+ comparison, HUC inference). **Full operational guide:** `Python_Code_Analysis/DL_Pipeline_v2/factorial_experiment/EXECUTION.md` (design in `wetland_factorial_experiment_plan.md`). This section is the digest — defer to EXECUTION.md for edge cases.

> **Agent boundary:** Claude *prepares* these scripts; **the user runs** all GPU/long jobs. Nothing here auto-launches training, containers, or rsync.

### Config grammar `<label>_<lidar>_<spectral>`
- `<label>`: `fld` (field-verified) / `nwi` / `flddeg` (field degraded to NWI prevalence) — the training **label source**
- `<lidar>`: `nolidar` / `chm` (CHM only) / `chmret` (CHM + return-fraction bands)
- `<spectral>`: `leafon` (leaf-on NAIP only) / `leafoff` (+ leaf-off NAIP RGB+NIR)

The **8 configs** (feature ablations on field labels; label comparison on the full feature set): `fld_nolidar_leafon` (18ch), `fld_nolidar_leafoff` (22), `fld_chm_leafon` (19), `fld_chm_leafoff` (23), `fld_chmret_leafon` (22), `fld_chmret_leafoff` (26, the best deployable field model), `nwi_chmret_leafoff` (26), `flddeg_chmret_leafoff` (26). A `nwi`/`flddeg` config **trains** on its label source but is **evaluated against field labels** (same seed ⇒ same test patches). Single source of truth: `dl_experiment_config.py` (`--list`, `--emit <config>` for shell vars, no-arg = channel-matrix self-check).

### Two-node split (the core transfer pattern)
| Node | Role | Storage |
|---|---|---|
| **CPU / login node** | prep + analysis (stats, preflight, aggregation, plots) | `/ibstorage/anthony/NYS_Wetlands_DL` = **canonical source of truth** |
| **GPU node** `cbsugpu09` / `cbsugpu10` (RTX A6000 48 GB) | training + eval + SHAP + predict, via `docker1` inside `tmux` | `/workdir/$USER` = **local, per-node** working copy |

**The GPU node has NO shared mount to `/ibstorage`, and `/workdir` is local to each GPU node** — files written during a reservation do not appear on the CPU node by themselves. Bridge the gap with **rsync over ssh**. Rules: containers may **only mount paths under `/workdir/$USER`**; always use **`docker1`** (BioHPC privileged wrapper), never bare `docker`; run long jobs under **`tmux`** (`screen` works too). The node copy lives at `/workdir/$USER/nys_wetlands` and mounts to `/app` (the image WORKDIR).

### Key scripts
**Python (`Python_Code_Analysis/DL_Pipeline_v2/`):** `dl_experiment_config.py` (config matrix, source of truth) · `dl_make_config_stats.py --all` (8 per-config stats from the master) · `dl_preflight_check.py --require-all-labels` (HARD GATE before GPU time) · `dl_08_aggregate_factorial.py` (Phase-3 aggregation, **CPU/pandas**) · `dl_08b_aggregate_patchcurve.py` (follow-on aggregation: patch-curve + `--arch-compare`, **CPU**) · `dl_09_shap_factorial.py` (SHAP, **GPU/in-container**) · `dl_huc_stack.py` (build HUC inference stack) · `dl_10_factorial_viz.ipynb` (lightweight figures §1-§7 from CSV/JSON, **CPU**, git-syncable) · `dl_10b_huc_inference_viz.ipynb` (data-heavy §8 from prediction GeoTIFFs, **CPU**).

**Shell (`Shell_Scripts/`):** `run_config.sh <config> <seed>` (one cell; idempotent — skips if `metrics.json`+`manifest.json` exist) · `run_factorial.sh` (top-level driver over all 24 cells; resumable) · `run_<config>.sh` (one config's seeds) · `run_patchcurve.sh` / `run_arch_compare.sh` / `run_predict_factorial.sh` (follow-on drivers) · `run_shap_factorial.sh` · `run_aggregate.sh` (wraps `dl_08`) · `rsync_results.sh` (pull results GPU→CPU) · `rsync_huc_sources.sh <cluster> <huc>` (pull per-HUC source rasters) · `run_tensorboard.sh`. The 1:1:1 mapping **config name = shell wrapper = `results/` folder** is the whole naming scheme.

Env knobs (override on CLI): `CONFIGS`, `SEEDS` (default `0 1 2`), `EPOCHS` (50), `BATCH_SIZE` (16), `BASE_FILTERS`/`DEPTH` (64/5), `PRECISION` (16-mixed), `RESULTS_DIR`, `DRY_RUN=1` (print commands, train nothing). Follow-on knobs: `ARCH`, `N_PATCHES`, `LEVELS`, `CELL_NAME`, `CAT_CHANNELS`, `DEEP_SUPERVISION` (all default to base-factorial behavior, so the 24-cell grid is untouched).

### End-to-end command flow
**1 · CPU prep** (env: `source .venv/bin/activate` or `conda activate wetland-cnn`):
```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
PIPE=Python_Code_Analysis/DL_Pipeline_v2
python $PIPE/dl_experiment_config.py                    # channel matrix self-check
python $PIPE/dl_make_config_stats.py --all              # -> Data/Training_Data/stats/*.json
python $PIPE/dl_preflight_check.py --require-all-labels  # must be GREEN before GPU time
```
Stats chain: R full-raster scan → `HUC_DL_Stacks_Extracted_Values.json` → `dl_01_compute_statistics.py --global-stats --weight-power 0.5` → master `multiclass_normalization_stats_wp0.5.json` → `dl_make_config_stats.py --all` → the 8 per-config files. Rebuild the master only when the R scan / bands / patches changed.

**2 · Build/load the Docker image** (build on a Docker host, e.g. Mac, amd64):
```bash
docker build --platform linux/amd64 -t nys-wetlands-dl . && docker save nys-wetlands-dl | gzip > nys-wetlands-dl.tar.gz
scp nys-wetlands-dl.tar.gz $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/   # then ON the GPU node:
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz                            # docker1, never docker
```

**3 · Push repo + data CPU → GPU** (FROM the CPU node — stage the **whole repo**, since the runner reads edited shell/Python and the per-config `stats/` and writes `results/`):
```bash
ssh $USER@cbsugpu10.biohpc.cornell.edu 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -av --exclude '.git' --exclude '.venv' \
  /ibstorage/anthony/NYS_Wetlands_DL/ \
  $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/
```
*Lean push* (skip the ~36 GB of `.ckpt`; for SHAP re-runs and follow-on studies, which only need code, `stats/`, patches, and the trained cells' `.safetensors`):
```bash
GPU_NODE=cbsugpu10.biohpc.cornell.edu
rsync -avhP --relative --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts \
  Data/Training_Data/stats Data/Training_Data/R_Patches_Merged \
  Models/factorial_results \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

**4 · Launch on the GPU node** (inside `tmux`, via `docker1`):
```bash
tmux new -s factorial          # Ctrl-b d to detach; tmux attach -t factorial to return
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  nys-wetlands-dl bash Shell_Scripts/run_factorial.sh
```
Flag rationale: `--gpus all` (A6000) · `--shm-size=8g` (DataLoader shared memory; 64 MB default → `bus error`) · `--user $(id -u):$(id -g)` (outputs owned by you, not root) · `-v …:/app` (only allowed mount; `/app` = WORKDIR) · `--rm` (disposable; state lives on the mount). Resume across reservations by rerunning the same command — completed cells are skipped. Each cell writes `results/<config>/seed<k>/`: `manifest.json`, `metrics.json`, `confusion_matrix.csv`, best `.safetensors`/`.ckpt`, `train.log`/`eval.log`.

**5 · Pull results GPU → CPU** (FROM the CPU node, via `rsync_results.sh` env vars):
```bash
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/results" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results" \
  Shell_Scripts/rsync_results.sh --metrics-only    # JSON/CSV/PNG only; drop flag for ~500 MB ckpts; -n to preview
```

**6 · Aggregation (CPU) & SHAP (GPU)** — the split mirrors the node split:
```bash
# Aggregation — CPU node, after sync-back (safe on a partial tree):
Shell_Scripts/run_aggregate.sh   # -> Models/factorial_results/analysis/{factorial_long,_summary,_table,contrasts,coverage}.csv + confusion_mean/
# SHAP — GPU node, in the container, BEFORE reservation teardown (backprops through each model):
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh --results-dir Models/factorial_results --force
#   --force is REQUIRED on a re-run (else every cell with an existing *_shap_importance.json is skipped).
```
Then `dl_10_factorial_viz.ipynb` renders the figures (runs under either the conda `wetland-cnn` kernel or the uv `nys-wetlands-dl (uv)` kernel — the uv `.venv` now has seaborn/ipykernel via `uv sync --extra notebooks`).

### Follow-on studies (EXECUTION §10)
Same node ritual: reload image, restage (lean push), run in container under `tmux`, sync back **under `Models/`**, aggregate on CPU. Each writes a **new results root**, so the base factorial is untouched.

- **Patch-count learning curve** → `Models/results_patchcurve/<config>_n<level>/seed<k>/` (6 levels × 3 seeds). GPU: `bash Shell_Scripts/run_patchcurve.sh fld_chmret_leafoff` (`LEVELS="100 200 300 400 500 full"`). CPU sync-back to `Models/results_patchcurve`, then `python …/dl_08b_aggregate_patchcurve.py --results-dir Models/results_patchcurve`.
- **UNet3+ architecture comparison** → `Models/results_arch/<config>_unet3plus/seed<k>/` (3 seeds, deep-supervision ON, bf64/d5 held = fair vs U-Net). GPU: `bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff` (`BATCH_SIZE` 8→4 on OOM). The U-Net arm is the base factorial's `Models/factorial_results/<config>/`. CPU: `python …/dl_08b_aggregate_patchcurve.py --arch-compare --config fld_chmret_leafoff --unet-dir Models/factorial_results --unet3plus-dir Models/results_arch`.
- **HUC prediction / inference maps** → `Data/HUC_DL_Predictions/DLpred_<mode>_cluster_<C>_huc_<H>.tif` (class) + `…_probs.tif` (per-class softmax). Needs the **source rasters** for the HUC, which live **outside** `/app` at `/workdir/$USER/NYS_Wetlands_Data` → use a **two-mount** wrapper (`-v …/NYS_Wetlands_Data:/data -e DATA_ROOT=/data`). Pull only the ~7 per-HUC tiles first: `SERVER="$USER@$DATA_HOST:" REMOTE_ROOT=/ibstorage/anthony/NYS_Wetlands_Data LOCAL_ROOT=/workdir/$USER/NYS_Wetlands_Data bash Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>`. Predict: `bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>` (best-macro-F1 seed default; arch auto-detected). Pull predictions back with a plain `rsync …/Data/HUC_DL_Predictions/ Data/HUC_DL_Predictions/`. Visualize in GIS locally; `dl_10b_huc_inference_viz.ipynb` reports per-class area / wetland-prevalence summary stats (reads the prediction GeoTIFFs directly, so rsync them to `Data/HUC_DL_Predictions/` first — they are gitignored).

`dl_10_factorial_viz.ipynb` §6–§7 visualize the first two studies (learning curves, U-Net vs UNet3+); §1–§5 cover the base factorial + SHAP. HUC prevalence (§8) lives in the data-heavy `dl_10b_huc_inference_viz.ipynb`. **Notebook split for git sync:** `dl_10` reads only small CSV/JSON (analysis `*.csv` + per-seed `metrics.json`/`training_log.json`/`training_history_*.json`/`shap/*_shap_importance.json`), which `.gitignore` whitelists (~2 MB) so they sync to a local Mac via `git pull`; `dl_10b` reads the multi-GB prediction GeoTIFFs, which stay gitignored and are rsync'd separately. Re-run `git add -A` after each aggregation to pick up new JSON/CSV.

## Production Model (the single deployable model)
Where the factorial asks *which inputs and labels matter*, this workstream ships **one** model. It is a **sibling workstream on the same trunk, not a branch** — it reuses `dl_01`–`dl_06`, the dataset, model, and losses untouched. **Full guide:** `Python_Code_Analysis/DL_Pipeline_v2/production_model/EXECUTION.md`; rationale + open decisions in `production_model/PLAN.md`.

- **Recipe (single source of truth):** `production_model/dl_prod_config.py` — run it with no args to print + self-check. Currently `nwifield_chmret_leafoff`, multiclass, unet bf64/d5, 100 epochs, seeds 0-2. The config was picked on factorial-v2 field-test results (best WET IoU **and** recall); every other knob is **held at the factorial's values on purpose**, so the benchmark's ranking remains valid evidence for the shipped model. `--emit` prints shell-sourceable `PROD_*` vars.
- **Driver:** `Shell_Scripts/run_production.sh [seed ...]` — a **thin wrapper over `run_config.sh`**, not a second training path, so fixes to training/metric-extraction land once. Inherits the skip-completed guard (safe stop/resume) and all of `run_config.sh`'s env knobs. `DRY_RUN=1` to plan.
- **Results:** `Models/production_model/<mode>/production/seed<k>/` — same layout as a factorial cell. Evaluation is still **against field labels** on the seed's held-out field patches, so scores are directly comparable to `Models/factorial_results_v2/analysis/cross_mode_summary.csv`.
- **Weights are the deliverable here** (unlike the factorial): pull the `.safetensors` back, not just `--metrics-only`. `.gitignore` tracks this root's `metrics.json`/`manifest.json`/`confusion_matrix.csv` and ignores the checkpoints.
- **Still open (do not silently decide):** ship best seed vs. 3-model ensemble; whether to refit on the full pool for the final artifact (forfeits the held-out score). See PLAN.md §4.
