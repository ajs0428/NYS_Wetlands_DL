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
  dl_03_mbfusion_model.py                 # Multi-branch fusion encoder (per-modality branches + per-pixel gate; --arch mbfusion)
  dl_model_factory.py                     # build_net() architecture dispatch (unet | unet3plus | mbfusion)
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
  dl_experiment_config.py                 # Factorial: 8-config matrix + fusion branch partition (single source of truth)
  dl_patch_pools.py                       # Factorial: field-anchored split + directory-aware leakage guard
  dl_preflight_check.py                   # Factorial: HARD GATE before GPU time (incl. [9] fusion branch partition)
  dl_11_export_gates.py                   # Factorial: [mbfusion] per-scale branch-gate rasters
  dl_08_aggregate_factorial.py            # Factorial: Phase-3 aggregation (CPU/pandas)
  dl_08b_aggregate_patchcurve.py          # Follow-on: patch-curve + --arch-compare aggregation (CPU)
  dl_09_shap_factorial.py                 # Factorial: per-cell SHAP (GPU, in-container)
  dl_10_factorial_viz.ipynb               # Lightweight figures §1-§7 (CSV/JSON only; git-syncable; wetland-cnn or uv kernel)
  dl_10b_huc_inference_viz.ipynb          # Data-heavy §8: HUC prevalence from prediction GeoTIFFs (rsync rasters separately)
  dl_huc_stack.py                         # Follow-on: build per-HUC inference stack
  factorial_experiment/PLAN.md            # Factorial: THE design doc (v3; consolidates v1+v2+arch_fusion plans)
  factorial_experiment/EXECUTION.md       # Factorial: THE runbook (v3; consolidates v1+v2 EXECUTION) -- read this
  factorial_experiment/archive/           # Superseded v1/v2 plans + runbooks, kept for provenance
  production_model/dl_prod_config.py      # Production: the shipped model's recipe (single source of truth)
  production_model/PLAN.md                # Production: recipe rationale + open decisions
  production_model/EXECUTION.md           # Production: operational walkthrough (deltas from factorial)
Shell_Scripts/                         # Orchestration wrappers (run_factorial.sh, run_*.sh, rsync_*.sh)
Models/production_model/               # Production: the deployable model's cells (+ its checkpoints)
webmap/                                # Leaflet/COG viewer + dev server (source tracked; COGs are not)
Models/factorial_results_v3/           # v3 base grid: <mode>/<config>/seed<k>/ + analysis/
Models/results_arch_v3/                # v3 arch arm 2: UNet3+ cells + analysis/
Models/results_arch_fusion_v3/         # v3 arch arm 3: mbfusion cells (+ gates/) + analysis/
Data/HUC_DL_Predictions_v3/            # HUC inference GeoTIFFs (class + probs)
Models/factorial_results_v2/           # frozen v2 grid (26 ch); v1 = Models/factorial_results/
Models/                                # Trained checkpoints + evaluation outputs
Data/Training_Data/R_Patches/          # 256x256 field-labeled patches (21 bands: 20 predictors + MOD_CLASS)
Data/Training_Data/R_Patches_NWI/      # NWI labels, paired 1:1 to R_Patches locations
Data/Training_Data/R_Patches_NWIextra/ # NWI labels at extra locations in the same HUC12s
Data/Training_Data/stats/              # per-config x per-mode normalization stats (16 files)
pyproject.toml                         # Dependencies + uv config
```

## Repo & Branching
- **`main` is the trunk.** All work lands here. (History note: `factorial-experiment-pipeline` had drifted into being the de-facto trunk while `main` sat abandoned; merged back into `main` 2026-07-27.)
- **Short-lived feature branches only** — days, not months — for changes risky enough to want isolation (a `dl_02_dataset.py` refactor, a new architecture). Name them `feat/<thing>` / `fix/<thing>` and merge back fast.
- **Never branch to separate a workstream.** Workstreams here share the entire core (`dl_01`–`dl_06`, dataset, model, losses), so a long-lived parallel branch means every core fix is applied twice and reconciled at merge — and this repo's merges are `.ipynb`-JSON and `.gitignore` merges, the painful kind.
- **Separate workstreams by directory + results root + config instead**, keeping the 1:1:1 mapping: `<workstream>` = `Shell_Scripts/run_<workstream>.sh` = `Models/<workstream>/`, with a `<workstream>/` doc+config dir under `DL_Pipeline_v2/`. Current instances: `factorial_experiment` → `Models/factorial_results_v3/` (+ `results_arch_v3/`, `results_arch_fusion_v3/`); `production_model` → `Models/production_model/`.
- **Freeze a published result with a tag, not a branch** (`factorial-v1`, `factorial-v2`). Tags are immutable and need no maintenance.
- **`.gitignore` starts with a blanket `*`** — anything new is invisible until explicitly whitelisted, and reaching a deep file requires un-ignoring every parent dir first. Check with `git check-ignore -v --no-index <path>` (without `--no-index` it silently skips already-tracked files).

## Pipeline Workflow
Run scripts in order: dl_01 -> dl_02 (imported by dl_04) -> dl_03 (imported by dl_04) -> dl_04_train_lightning -> dl_05 -> dl_06
- **Primary training:** `dl_04_train_lightning.py` (Lightning Trainer with callbacks)
- **Legacy training:** `dl_04_train.py` (manual loop, kept as fallback)
- **Architecture:** selectable via `--arch` — `unet` (default, `dl_03_unet_model.py`, residual blocks + SE attention), `unet3plus` (`dl_03_unet3plus_model.py`, full-scale skips + deep supervision), or `mbfusion` (`dl_03_mbfusion_model.py`, per-modality branch encoders + per-pixel gated fusion). Dispatched by `dl_model_factory.build_net()`.

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
- **mbfusion** (`--arch mbfusion`): one encoder per input CATEGORY (terrain / lidar / leafon / leafoff), fused at every scale by a per-pixel softmax gate (`BranchFusion`: per-branch GroupNorm → 3x3 gate → weighted concat → 1x1 proj), into a decoder **bit-identical** to the U-Net's. Branch map is derived from `stats["predictor_names"]` in post-one-hot channel space and serialized to the checkpoint — never a CLI knob. 162M params at bf64/d5/29ch (~1.3x U-Net); activation-bound, so prefer `BATCH_SIZE=8`. Gate entropy is logged per scale for collapse monitoring.
- Input: **29 channels** for the full v3 feature set (20 predictor bands; `Geomorph_local` one-hot expands 1 band to 10 channels). Reduced-band factorial configs are 21 or 25 — `dl_experiment_config.py` is the source of truth. (v2 was 26; older single-model checkpoints may be 31.)
- Loss: Hybrid Focal + Dice with class weights (in `dl_losses.py`)
- Optimizer: AdamW + ReduceLROnPlateau
- Callbacks: ModelCheckpoint, EarlyStopping, LearningRateMonitor
- Patch size must be divisible by 2^depth (16 for depth=4, 32 for depth=5)

## Data Notes
- Training patches: 256x256 pixels, **21 bands** (20 predictors + 1 label `MOD_CLASS`) as of v3. Three label sources = three parallel directories with an identical predictor schema: `R_Patches` (field), `R_Patches_NWI` (NWI, paired 1:1 by filename `NWI_` + field basename), `R_Patches_NWIextra` (NWI, extra locations in the same HUC12s, geographically disjoint from field).
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
- **Predictor bands (20 -> 29 channels)** — the v3 set. Authoritative order is `predictor_names` in the master stats file, **not** raster band order:
  - Terrain (7): DEM, slope_local, **TPI_local**, **meanc_local**, **dmv_local**, flowacc, twi
  - Geomorphon (1): Geomorph_local (one-hot encoded to 10 channels — this is the +9)
  - LiDAR structure (4): CHM, pct_below_1m, pct_1m_to_5m, pct_above_5m  *(note the `_1m`/`_5m` names; the older `_0.5m`/`_2m` names are v1-era)*
  - NAIP leaf-on (4): r, g, b, nir
  - NAIP leaf-off (4): r_lo, g_lo, b_lo, nir_lo
  - **v3 added** `TPI_local`, `meanc_local`, `dmv_local` upstream in `NYS_Wetlands_Data/` (`step_terrain.sh`), taking patches 18 -> 21 bands and the full feature set 26 -> **29 channels**. No SAR / Sentinel-2 / EVI / NDYI / GDVI / n_ndvi / n_ndwi in this stack — those belong to the older single-model generation and are not part of the factorial.
- **Normalization:** min_max bands use global raster statistics (`--global-stats` flag in dl_01) so normalization covers the full inference range. Geomorph_local uses one-hot encoding (10 classes). Class weights are power-scaled at a **fixed `--weight-power 0.5`** (the `_wp0.5` filename suffix), recomputed per config/mode from the active label source's pixel counts.
- **Label band:** MOD_CLASS (provenance is the *directory*, not the band name)
- Classes: 0=EMW, 1=FSW, 2=SSW, 3=UPL, 255=unlabeled (OWW removed; can be re-added via dl_band_config.json). Binary mode collapses EMW/FSW/SSW -> WET via `binary_mapping`.
- **NWI "non-wetland" = confirmed UPL (class 3)**, not ignore_index — so NWI omission errors enter training as realistic label noise, which is the operational case the experiment measures. `255` is reserved for genuinely unlabeled pixels, and its mask must match between paired field/NWI patches.
- **Class balance:** roughly UPL-dominant (~75%) with FSW the largest wetland class. Exact per-directory prevalence is **measured and reported by `dl_preflight_check.py`** — read it there rather than trusting a cached number, since it shifts as patches are added.
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

## Factorial Experiment & Follow-on Studies (v3)
A controlled benchmark isolating the effect of **LiDAR tier**, **leaf-off NAIP**, **label source**, and **classification mode** on wetland segmentation, plus a three-arm architecture comparison and HUC inference. **Full design:** `Python_Code_Analysis/DL_Pipeline_v2/factorial_experiment/PLAN.md`. **Full runbook:** the sibling `EXECUTION.md`. Superseded v1/v2 docs live in `factorial_experiment/archive/`. This section is the digest — defer to those two for edge cases.

**The grid: 8 configs × 2 modes × 5 seeds = 80 cells**, plus 10 cells per extra architecture arm (20) = **100 cells**. Every cell is `Models/<root>_v3/<mode>/<cell>/seed<k>/`.

> **Agent boundary:** Claude *prepares* these scripts; **the user runs** all GPU/long jobs. Nothing here auto-launches training, containers, or rsync.

> **v3 status (2026-08-25): not yet run.** Code is built and CPU-tested; patches and stats are being rebuilt. Preflight GREEN is the hard gate before GPU time.

### Config grammar `<label>_<lidar>_<spectral>`
- `<label>`: `fld` (field-verified) / `nwi` (NWI paired to field locations) / `nwiextra` (NWI ∪ extra same-HUC12 locations, ~2×) / `nwifield` (field ∪ non-overlapping NWI) / `flddeg` (field degraded to NWI prevalence) — the training **label source**, which in v2+ is a **separate patch directory**, not a label band
- `<lidar>`: `nolidar` / `chmret` (CHM + return fractions). **Two tiers** — the v1 CHM-only tier was dropped, so the axis contrasts *no structure* vs. *full structure*
- `<spectral>`: `leafon` (leaf-on NAIP only) / `leafoff` (+ leaf-off NAIP RGB+NIR)

The **8 configs** (feature ablations on field labels; label comparison on the full feature set) with **v3 channel counts** — three terrain metrics (`TPI_local`, `meanc_local`, `dmv_local`) were added upstream, moving every config up 3 channels from v2's 18/22/26:

| Config | v3 | v2 | | Config | v3 | v2 |
|---|---|---|---|---|---|---|
| `fld_nolidar_leafon` | 21 | 18 | | `nwi_chmret_leafoff` | 29 | 26 |
| `fld_nolidar_leafoff` | 25 | 22 | | `nwiextra_chmret_leafoff` | 29 | 26 |
| `fld_chmret_leafon` | 25 | 22 | | `nwifield_chmret_leafoff` | 29 | 26 |
| `fld_chmret_leafoff` | **29** | 26 | | `flddeg_chmret_leafoff` | 29 | 26 |

`fld_chmret_leafoff` is the full feature set and the **channel anchor** the preflight asserts against the master stats. Each config runs in **both** modes (`multiclass` EMW/FSW/SSW/UPL, `binary` WET/UPL). A non-`fld` config **trains** on its label source but is **evaluated against field labels** — same seed ⇒ same test patches, always drawn from `R_Patches`. **Single source of truth: `dl_experiment_config.py`** (`--list`, `--emit <config> --mode <mode>` for shell vars, no-arg = channel-matrix self-check). Never hardcode this table.

**Split & leakage (the silent-failure guard).** The split is computed once on `R_Patches` per seed; `test_fld` is the test set for every config and mode; each config's train/val pool is drawn from its own directory and filtered clear of `test_fld` by the *source-appropriate* key — filename pairing (`nwi_field_twin()`) for `fld`/`nwi`/`flddeg`, HUC12 geography (`huc12_of()`) for `nwiextra`/`nwifield`. `LEAKAGE_GUARD=huc12` (default, headline) / `coord` (sensitivity). `dl_patch_pools.resolve_pools()` is the one place this lives, and the preflight imports it — so preflight and training agree by construction. **Never key patches by the `cluster_..._patch_N` substring** — it drops the identity-bearing source prefix (`ADK_WCT_AJS_`, `gps_jc_`, …) and is neither unique within a directory nor comparable across them (measured on the v2 set: 56 collisions in `R_Patches`, 594 spurious `NWIextra` "matches").

### Two-node split (the core transfer pattern)
| Node | Role | Storage |
|---|---|---|
| **CPU / login node** | prep + analysis (stats, preflight, aggregation, plots) | `/ibstorage/anthony/NYS_Wetlands_DL` = **canonical source of truth** |
| **GPU node** `cbsugpu09` / `cbsugpu10` (RTX A6000 48 GB) | training + eval + SHAP + predict, via `docker1` inside `tmux` | `/workdir/$USER` = **local, per-node** working copy |

**The GPU node has NO shared mount to `/ibstorage`, and `/workdir` is local to each GPU node** — files written during a reservation do not appear on the CPU node by themselves. Bridge the gap with **rsync over ssh**. Rules: containers may **only mount paths under `/workdir/$USER`**; always use **`docker1`** (BioHPC privileged wrapper), never bare `docker`; run long jobs under **`tmux`** (`screen` works too). The node copy lives at `/workdir/$USER/nys_wetlands` and mounts to `/app` (the image WORKDIR).

### Key scripts
**Python (`Python_Code_Analysis/DL_Pipeline_v2/`):** `dl_experiment_config.py` (config matrix + fusion branch partition, source of truth) · `dl_patch_pools.py` (field-anchored split + leakage guard) · `dl_make_config_stats.py --all --mode <mode>` (8 per-config stats per mode, from the master) · `dl_preflight_check.py` (**HARD GATE**; `--seeds` defaults to R=5) · `dl_08_aggregate_factorial.py` (base-grid aggregation, **CPU/pandas**) · `dl_08b_aggregate_patchcurve.py` (`--arch-compare` across N arms via repeatable `--arch-dir name=path`, **CPU**) · `dl_09_shap_factorial.py` (SHAP, **GPU/in-container**) · `dl_11_export_gates.py` (`[mbfusion]` gate rasters) · `dl_huc_stack.py` / `dl_06b_predict_huc.py` (HUC inference).

**Shell (`Shell_Scripts/`):** `run_config.sh <config> <seed>` (one cell; idempotent — skips if `metrics.json`+`manifest.json` exist) · `run_factorial.sh` (top-level driver over all 80 cells, mode-outer; resumable) · `run_<config>.sh` (one config's seeds) · `run_arch_compare.sh` / `run_arch_fusion.sh` (the two extra arch arms) · `run_predict_factorial.sh` · `run_shap_factorial.sh` · `run_aggregate.sh` (wraps `dl_08`) · `rsync_results.sh` (pull results GPU→CPU) · `rsync_huc_sources.sh <cluster> <huc>` · `run_tensorboard.sh`. `run_patchcurve.sh` exists but is **not part of v3**. The 1:1:1 mapping **config name = shell wrapper = `<mode>/<config>/` results folder** is the whole naming scheme.

Env knobs (override on CLI, or via `-e` into the container): `MODES` (`multiclass binary`), `MODE`, `CONFIGS`, `SEEDS` (**default `0 1 2 3 4`**), `EPOCHS` (50), `BATCH_SIZE` (16 base / 8 arch arms), `BASE_FILTERS`/`DEPTH` (64/5), `PRECISION` (16-mixed), `LEAKAGE_GUARD` (huc12), `STATS_DIR`, `RESULTS_DIR` (`Models/factorial_results_v3`; the `<mode>/` level is appended by `run_config.sh`), `DRY_RUN=1`. Arch knobs: `ARCH` (`unet`|`unet3plus`|`mbfusion`), `CELL_NAME`, `CAT_CHANNELS`, `DEEP_SUPERVISION`, `GATE_KERNEL`.

> **Knobs must go through `-e` when running in the container.** A host-side prefix (`MODE=binary docker1 run …`) sets the variable in *your shell*, not the container — the script silently falls back to its defaults. Paths in `-e` values are **container** paths (`/app/…`).

### End-to-end command flow
**1 · CPU prep** (env: `conda activate wetland-cnn`, or `source .venv/bin/activate` where present):
```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
PIPE=Python_Code_Analysis/DL_Pipeline_v2
python $PIPE/dl_experiment_config.py                    # channel matrix self-check (21/25/29)
python $PIPE/dl_01_compute_statistics.py --patches-dir Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json --weight-power 0.5
python $PIPE/dl_make_config_stats.py --all --mode multiclass
python $PIPE/dl_make_config_stats.py --all --mode binary    # 16 per-config files
python $PIPE/dl_preflight_check.py                      # must be GREEN before GPU time
```
Stats chain: R full-raster scan → `HUC_DL_Stacks_Extracted_Values.json` → `dl_01 --global-stats --weight-power 0.5` → master `multiclass_normalization_stats_wp0.5.json` → `dl_make_config_stats.py --all --mode <m>` → the 8 per-config files per mode. Rebuild the master whenever the R scan / bands / **patch count** changed (the count feeds field class weights) — all three changed for v3. `dl_make_config_stats` derives **both** modes' normalization from the *multiclass* master and recomputes binary class weights from disk.

Preflight asserts: directory + predictor parity, field↔NWI pairing 1:1 and footprint identity, `NWIextra` HUC12s ⊆ field, label values ⊆ `{0,1,2,3,255}` (binary → `{0,1,255}`), NWI 255-mask == field mask, **no `test_fld` key in any config's train/val pool**, per-config channels + stats presence, and **[9] the fusion branch partition** (slices disjoint, cover all 29 channels, one-hot block contiguous).

**2 · Build/load the Docker image** (build on a Docker host, e.g. Mac, amd64):
```bash
docker build --platform linux/amd64 -t nys-wetlands-dl . && docker save nys-wetlands-dl | gzip > nys-wetlands-dl.tar.gz
scp nys-wetlands-dl.tar.gz $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/   # then ON the GPU node:
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz                            # docker1, never docker
```

**3 · Push repo + data CPU → GPU** (FROM the CPU node — stage the **whole repo**, since the runner reads edited shell/Python and the per-config `stats/` and writes results):
```bash
ssh $USER@cbsugpu10.biohpc.cornell.edu 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -av --exclude '.git' --exclude '.venv' \
  /ibstorage/anthony/NYS_Wetlands_DL/ \
  $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/
```
*Lean push* (skip the `.ckpt`; code + stats + the **three** patch dirs is everything a fresh v3 run needs):
```bash
GPU_NODE=cbsugpu10.biohpc.cornell.edu
rsync -avhP --relative --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts Data/Training_Data/stats \
  Data/Training_Data/R_Patches Data/Training_Data/R_Patches_NWI Data/Training_Data/R_Patches_NWIextra \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```
**Always restage** even if `/workdir/$USER/nys_wetlands` survived the last reservation, or you run stale wrappers.

**4 · Launch on the GPU node** (inside `tmux`, via `docker1`):
```bash
tmux new -s factorial_v3       # Ctrl-b d to detach; tmux attach -t factorial_v3 to return
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  nys-wetlands-dl bash Shell_Scripts/run_factorial.sh
```
Flag rationale: `--gpus all` (A6000) · `--shm-size=8g` (DataLoader shared memory; 64 MB default → `bus error`) · `--user $(id -u):$(id -g)` (outputs owned by you, not root) · `-v …:/app` (only allowed mount; `/app` = WORKDIR) · `--rm` (disposable; state lives on the mount). The driver's outer loop is `MODES`, so a stopped run still yields a complete multiclass factorial. Resume by rerunning the same command — completed cells are skipped. Each cell writes `Models/factorial_results_v3/<mode>/<config>/seed<k>/`: `manifest.json` (bands, in_channels, mode, label source, patch dirs, pool rule, leakage regime, class weights, arch, git commit), `metrics.json`, `confusion_matrix.csv`, `training_log.json`, best `.safetensors`/`.ckpt`, `train.log`, `tb_logs/`. **There is no separate `dl_05` eval step** — the trainer evaluates on the field test set and `run_config.sh` extracts the metrics from its journal.

**5 · Pull results GPU → CPU** (FROM the CPU node, via `rsync_results.sh` env vars):
```bash
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v3" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v3" \
  Shell_Scripts/rsync_results.sh --metrics-only    # JSON/CSV/PNG/NPZ/LOG; drop flag for ckpts; -n to preview
```
Keep the `*.log`: Lightning's model summary in `train.log` is the only source of the GFLOPs column in `arch_cost.csv`.

**6 · Aggregation (CPU) & SHAP (GPU)** — the split mirrors the node split, and both run **once per mode**:
```bash
# Aggregation — CPU node, after sync-back (safe on a partial tree):
RESULTS_DIR=Models/factorial_results_v3/multiclass Shell_Scripts/run_aggregate.sh
RESULTS_DIR=Models/factorial_results_v3/binary     Shell_Scripts/run_aggregate.sh
#   -> <root>/analysis/{factorial_long,factorial_summary,factorial_table,contrasts,coverage}.csv + confusion_mean/
# SHAP — GPU node, in the container, BEFORE reservation teardown (backprops through each model):
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh --results-dir Models/factorial_results_v3/multiclass
#   ... --results-dir Models/factorial_results_v3/binary --mode binary
#   --force is REQUIRED on a re-run (else every cell with an existing *_shap_importance.json is skipped).
```
Contrasts are **paired by seed** (same seed ⇒ same split across all 8 configs), covering the LiDAR tier, the leaf-off main effect, their interaction, and the **label gradient** `nwi → nwiextra → nwifield → flddeg → fld`. SHAP writes per-band importance in **both** aggregations — SUM over a band's channels *and* per-channel MEAN — because the one-hot `Geomorph_local` band is 10 channels while every continuous band is 1, so the SUM inflates it ~10×.

### Architecture comparison — three arms (v3's headline addition)
One config (`fld_chmret_leafoff`, 29 ch), both modes, **the same 5 seeds**, bf64/d5/50-epochs held, so the only variable is the network. **No arm is pre-trained** — the v2 arms are 26-channel runs on the old pool and are not comparable, so all three retrain.

| Arm | Driver | Root |
|---|---|---|
| U-Net | the base grid itself | `Models/factorial_results_v3/<mode>/fld_chmret_leafoff/` |
| UNet3+ | `run_arch_compare.sh <config>` | `Models/results_arch_v3/<mode>/<config>_unet3plus/` |
| **`mbfusion`** | `run_arch_fusion.sh <config>` | `Models/results_arch_fusion_v3/<mode>/<config>_mbfusion/` |

**`mbfusion`** (`dl_03_mbfusion_model.py`) gives each input category its own encoder — terrain (17 ch, width 48) / lidar (4, 32) / leafon (4, 32) / leafoff (4, 32) → **144 fused** — and fuses them at all 6 scales with a per-pixel softmax gate, into a decoder **bit-identical** to the U-Net's. 162 M params vs the U-Net's 125 M (~1.3×), but the binding constraint is *activations* (2.25× at the finest scale), so `BATCH_SIZE` defaults to 8; halve to 4 on OOM. **The branch map is not a knob** — the trainer derives it from `stats["predictor_names"]` in post-one-hot channel space and stores it in the checkpoint, so eval/predict auto-detect it; preflight **[9]** gates it. Watch `train/gate_entropy/level0..5` in TensorBoard for gate collapse (healthy ≈ `log(4)` = 1.386). Export gate rasters before teardown with `dl_11_export_gates.py`.

Aggregate all three at once — arm count follows from the CLI, once per mode:
```bash
python $PIPE/dl_08b_aggregate_patchcurve.py --arch-compare --config fld_chmret_leafoff --mode multiclass \
  --arch-dir unet=Models/factorial_results_v3 --arch-dir unet3plus=Models/results_arch_v3 \
  --arch-dir mbfusion=Models/results_arch_fusion_v3
#   -> arch_compare_long.csv (tidy; plot from this) + arch_contrasts.csv + arch_cost.csv + arch_compare.csv
```
`--confusion-pair` (default `FSW UPL`) adds **row-normalized** directional confusion rates — the share of true-FSW pixels predicted UPL — which is the specific failure the fusion encoder targets. **At n=5 the credible summary is sign consistency** (`n_better`/`n_seeds`); no p-values are computed, deliberately. **Reading gate maps:** within-branch *spatial* comparison is valid; cross-branch *absolute* comparison is confounded (after gating, `proj` is a 1×1 conv, so a branch with modest gates but large weights can still dominate) — take overall branch importance from SHAP instead.

### Follow-on studies
Same node ritual: reload image, restage (lean push), run in container under `tmux`, sync back **under `Models/`**, aggregate on CPU. Each writes a **new results root**, so the base grid is untouched.

- **HUC prediction / inference maps** → `Data/HUC_DL_Predictions_v3/DLpred_<mode>_cluster_<C>_huc_<H>.tif` (class) + `…_probs.tif` (per-class softmax). Needs the **source rasters** for the HUC, which live **outside** `/app` at `/workdir/$USER/NYS_Wetlands_Data` → use a **two-mount** wrapper (`-v …/NYS_Wetlands_Data:/data -e DATA_ROOT=/data`). Pull only the ~7 per-HUC tiles first: `SERVER="$USER@$DATA_HOST:" REMOTE_ROOT=/ibstorage/anthony/NYS_Wetlands_Data LOCAL_ROOT=/workdir/$USER/NYS_Wetlands_Data bash Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>`. Predict: `bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>` (best-macro-F1 seed default; arch auto-detected, so UNet3+/`mbfusion` cells work unchanged). Batch many HUCs from `Shell_Scripts/huc.txt` (`<cluster>:<huc>` per line). Pull predictions back with a plain `rsync`.
- **Patch-count learning curve — dropped from v3.** The 100–500 range is under one order of magnitude and won't support a scaling claim; deferred until the pool reaches 1000s of patches. `run_patchcurve.sh` and `dl_08b`'s curve mode are unchanged so the v2 curve reproduces.

**Figures:** `R_Code_Analysis/dl_10_Factorial_viz_R.qmd` is the **active** viz notebook (its architecture section reads `arch_compare_long.csv` and is arm-count-agnostic). The Python `dl_10_factorial_viz.ipynb` (§1–§5 base + SHAP, §6–§7 follow-ons, `MODE` selector) and `dl_10b_huc_inference_viz.ipynb` (§8 HUC prevalence) are the older siblings. **Notebook split for git sync:** `dl_10` reads only small CSV/JSON, which `.gitignore` whitelists (~2 MB) so they sync to a local Mac via `git pull`; `dl_10b` reads the multi-GB prediction GeoTIFFs, which stay gitignored and are rsync'd separately. Re-run `git add -A` after each aggregation.

> **Two schema facts for any new analysis code.** v2/v3 `metrics.json` nests scores under **`"test_metrics"`** (v1 had them flat) — unwrap with `scores = metrics.get("test_metrics") or metrics`. And `confusion_matrix` is a **dict** `{"labels": [...], "matrix": [[...]]}` at top level (v1 was a bare nested list) — `np.array(cm)` on it raises. `dl_08`/`dl_08b` already handle both.
## Production Model (the single deployable model)
Where the factorial asks *which inputs and labels matter*, this workstream ships **one** model. It is a **sibling workstream on the same trunk, not a branch** — it reuses `dl_01`–`dl_06`, the dataset, model, and losses untouched. **Full guide:** `Python_Code_Analysis/DL_Pipeline_v2/production_model/EXECUTION.md`; rationale + open decisions in `production_model/PLAN.md`.

- **Recipe (single source of truth):** `production_model/dl_prod_config.py` — run it with no args to print + self-check. Currently `nwifield_chmret_leafoff`, multiclass, unet bf64/d5, 100 epochs, seeds 0-2. The config was picked on factorial-v2 field-test results (best WET IoU **and** recall); every other knob is **held at the factorial's values on purpose**, so the benchmark's ranking remains valid evidence for the shipped model. `--emit` prints shell-sourceable `PROD_*` vars.
- **Driver:** `Shell_Scripts/run_production.sh [seed ...]` — a **thin wrapper over `run_config.sh`**, not a second training path, so fixes to training/metric-extraction land once. Inherits the skip-completed guard (safe stop/resume) and all of `run_config.sh`'s env knobs. `DRY_RUN=1` to plan.
- **Results:** `Models/production_model/<mode>/production/seed<k>/` — same layout as a factorial cell. Evaluation is still **against field labels** on the seed's held-out field patches, so scores are directly comparable to `Models/factorial_results_v2/analysis/cross_mode_summary.csv`.
- **Weights are the deliverable here** (unlike the factorial): pull the `.safetensors` back, not just `--metrics-only`. `.gitignore` tracks this root's `metrics.json`/`manifest.json`/`confusion_matrix.csv` and ignores the checkpoints.
- **Still open (do not silently decide):** ship best seed vs. 3-model ensemble; whether to refit on the full pool for the final artifact (forfeits the held-out score). See PLAN.md §4.
