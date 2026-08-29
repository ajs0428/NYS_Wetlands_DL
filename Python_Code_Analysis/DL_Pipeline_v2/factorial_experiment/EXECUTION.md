# Wetland Factorial Experiment v3 — Execution Guide

How to actually *run* the v3 factorial, end to end, on the BioHPC GPU nodes
(`cbsugpu09` / `cbsugpu10`) via `docker1`. This is the **single runbook** — it
consolidates the old `EXECUTION.md` (v1), `EXECUTION_v2.md`, and the operational
half of `arch_fusion/PLAN.md`, all of which are preserved under `archive/`.

Design and rationale live in [`PLAN.md`](PLAN.md). Commands live here.

> **Agent boundary (`AGENTS.md`).** Claude Code *prepares* these scripts; **you run
> them.** Nothing here auto-launches training, containers, or rsync.

> **Status (2026-08-25): v3 has not run yet.** All code is built and CPU-tested. The
> blocking item is CPU prep — the master stats are stale (§3, and it is a hard gate,
> not a nicety). Everything from §4 on is ready to execute.

---

## 0. The v3 flow, one map

| # | Stage | Node | § | Status |
|---|---|---|---|---|
| 1 | Prep: config self-check → **rebuild stats masters** → per-config stats → preflight GREEN | CPU | §3 | ⏳ **required** |
| 2 | Build/load the Docker image | Docker host → GPU | §4 | ⏳ |
| 3 | Stage repo + 3 patch dirs onto `/workdir` | CPU → GPU | §5 | ⏳ |
| 4 | Base grid: 8 configs × 2 modes × 5 seeds = **80 cells** | GPU | §6–§7 | ⏳ |
| 5 | SHAP per cell, in-container, before teardown | GPU | §8 | ⏳ |
| 6 | Arch arm 2 — UNet3+ (2 modes × 5 seeds = 10 cells) | GPU | §9 | ⏳ |
| 7 | Arch arm 3 — `mbfusion` (10 cells) + gate export | GPU | §9 | ⏳ |
| 8 | HUC inference maps | GPU | §10 | ⏳ |
| 9 | End-of-reservation sync-back: metrics, weights, GeoTIFFs → `/ibstorage` | CPU | §11 | ⏳ |
| 10 | Aggregation + viz | CPU | §8, §12 | ⏳ |

Invariants at every stage: every driver is **idempotent** (a completed cell is
skipped, so stop at a reservation's end and rerun next session);
`/ibstorage/anthony/NYS_Wetlands_DL` is canonical and the GPU `/workdir` copy is
disposable; the fresh `_v3` roots mean v1/v2 results are never touched; **the user
launches every GPU job.**

### 0.1 TL;DR

```bash
# --- CPU/login node: prep -----------------------------------------------
cd /ibstorage/anthony/NYS_Wetlands_DL
PIPE=Python_Code_Analysis/DL_Pipeline_v2
python $PIPE/dl_experiment_config.py                       # channel-matrix self-check
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power 0.5                                       # master (STALE -- see §3)
python $PIPE/dl_make_config_stats.py --all --mode multiclass
python $PIPE/dl_make_config_stats.py --all --mode binary
python $PIPE/dl_preflight_check.py                         # MUST be green

# --- CPU node: push the tree to the GPU node's local /workdir -----------
# Narrow push (~14 GB). NOT `rsync -av` of the repo -- that moves ~458 GB of
# v1/v2 checkpoints and prediction rasters a v3 run never reads. See §5.
GPU_NODE=cbsugpu10.biohpc.cornell.edu
bash Shell_Scripts/rsync_push_v3.sh -n          # preview, then drop -n

# --- GPU node: load image, launch the grid inside tmux ------------------
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz      # first time only
tmux new -s factorial_v3
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  nys-wetlands-dl bash Shell_Scripts/run_factorial.sh
# Ctrl-b then d to detach; tmux attach -t factorial_v3 to return

# --- CPU node: pull results back ---------------------------------------
SERVER="$USER@$GPU_NODE:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v3" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v3" \
  Shell_Scripts/rsync_results.sh --metrics-only            # -n to preview
```

---

## 1. Where work happens (the two-node split)

| Node | Role | What runs there |
|---|---|---|
| **CPU / login node** (`/ibstorage` = source of truth) | prep + analysis | stats, `dl_preflight_check.py`, aggregation, plots |
| **GPU node** `cbsugpu09` / `cbsugpu10` (1× RTX A6000 48 GB; local 7 TB `/workdir`) | training + eval + SHAP + predict | drivers inside `docker1`, inside `tmux` |

The GPU node is **not** connected to the CPU nodes — `/ibstorage` is not mounted
there, and `/workdir` is **local to each GPU node**, so files written during a
reservation do not appear on the CPU node by themselves. Bridge the gap with
**rsync over ssh**, not a mount: a given storage volume can only be mounted in one
place per node, so rsync is the more flexible and less fragile path. Hence the
explicit push out (§5) and pull back (§11).

**Hard BioHPC rules:** containers may mount **only** paths under `/workdir/$USER`;
always use **`docker1`** (the privileged site wrapper), never bare `docker`; run long
jobs under **`tmux`** (`screen` works too); scratch goes in `/workdir/$USER/tmp`, not
`/tmp`.

---

## 2. The script map

Two layers. **`dl_*` Python** = pipeline mechanism (built once, imported).
**`run_*.sh` shell** (in `Shell_Scripts/`) = orchestration you invoke. The name of
every shell wrapper *is* a config name *is* a results folder — that 1:1:1 mapping is
the whole naming scheme.

### 2a. Config grammar `<label>_<lidar>_<spectral>`

`<label>` ∈ {`fld`, `nwi`, `nwiextra`, `nwifield`, `flddeg`} ·
`<lidar>` ∈ {`nolidar`, `chmret`} · `<spectral>` ∈ {`leafon`, `leafoff`}.
So `fld_chmret_leafoff` = field labels, full LiDAR, both seasons — the full
**29-channel** feature set.

The 8 configs and their v3 channel counts:

| Config | `in_channels` (v3) | was (v2) |
|---|---|---|
| `fld_nolidar_leafon` | 21 | 18 |
| `fld_nolidar_leafoff` | 25 | 22 |
| `fld_chmret_leafon` | 25 | 22 |
| `fld_chmret_leafoff` | 29 | 26 |
| `nwi_chmret_leafoff` | 29 | 26 |
| `nwiextra_chmret_leafoff` | 29 | 26 |
| `nwifield_chmret_leafoff` | 29 | 26 |
| `flddeg_chmret_leafoff` | 29 | 26 |

× 2 modes × 5 seeds = **80 cells**. Three terrain metrics (`TPI_local`,
`meanc_local`, `dmv_local`) added upstream are what moved every config up by 3.
**Confirm against `python $PIPE/dl_experiment_config.py`, not this table** — that
file is the source of truth.

```bash
python $PIPE/dl_experiment_config.py --list                       # config names
python $PIPE/dl_experiment_config.py                              # channel self-check
python $PIPE/dl_experiment_config.py --emit nwifield_chmret_leafoff --mode binary
#  -> CONFIG MODE LABEL_SOURCE IN_CHANNELS PATCH_DIRS POOL_RULE FIELD_TEST_DIR
#     TRAIN_STATS EVAL_STATS
```

`PATCH_DIRS` + `POOL_RULE` tell the runner where train/val come from; **test is
always `FIELD_TEST_DIR` (`R_Patches`) at the seed's `test_fld` keys.** Every
config and both modes are scored on the same field pixels — the runner enforces this
by evaluating with the matching `fld_*` stats. Same seed ⇒ same test patches.

### 2b. Python mechanism (`Python_Code_Analysis/DL_Pipeline_v2/`)

| File | Role |
|---|---|
| `dl_experiment_config.py` | **Single source of truth.** Band matrix, 8 configs, the config→directory registry, `BRANCH_BANDS`/`BRANCH_WIDTHS`, and `branch_indices_from_predictors()`. |
| `dl_patch_pools.py` | `resolve_pools(config, seed, mode, leakage_guard)` — the field-anchored split + directory-aware leakage guard. The **one** place the guard lives; preflight imports it, so preflight and training agree by construction. |
| `dl_degrade_labels.py` | Seeded in-memory train/val degrade for `flddeg`. Auto-wired into the pools. |
| `dl_make_config_stats.py` | Subsets the master into the 8 per-config stats, per mode. `--all --mode <mode>`. |
| `dl_preflight_check.py` | **Hard gate before any GPU time.** Directory presence, predictor parity, field↔NWI pairing + footprint identity, label-value sanity, 255-mask match, split + leakage gate, per-config channels + stats presence, and **[9] the fusion branch partition**. |
| `dl_04_train_lightning.py` | Training. Takes `--config/--mode/--stats-dir/--leakage-guard`; **already evaluates on the field test set** and writes test metrics to `training_log.json`. |
| `dl_08_aggregate_factorial.py` | Base-grid aggregation (**CPU/pandas**). |
| `dl_08b_aggregate_patchcurve.py` | `--arch-compare` aggregation across N arms (**CPU**). |
| `dl_09_shap_factorial.py` | SHAP per cell (**GPU, in-container**). |
| `dl_11_export_gates.py` | `[mbfusion]` per-scale, per-branch gate rasters from a trained cell. Standalone — re-runs against any archived cell. |
| `dl_huc_stack.py` / `dl_06b_predict_huc.py` | In-memory per-HUC stack + sliding-window inference. |

> **There is no separate `dl_05` eval step in v2/v3.** The trainer evaluates on the
> field test set itself; `run_config.sh` extracts `metrics.json` +
> `confusion_matrix.csv` + `manifest.json` from the trainer's journal. (v1 called
> `dl_05_evaluate.py` — that path still exists but the factorial does not use it.)

### 2c. Shell orchestration (`Shell_Scripts/`)

| Script | What it does |
|---|---|
| `run_config.sh <config> <seed>` | **The workhorse.** Runs one cell: resolves stats via `--emit`, trains, extracts metrics/manifest, writes `RESULTS_DIR/<mode>/<cell>/seed<k>/`. **Idempotent** — skips a cell whose `metrics.json` + `manifest.json` exist. |
| `run_<config>.sh` (×8) | Thin wrapper: one config over `SEEDS`. |
| `run_factorial.sh` | **Top-level driver.** Walks (mode × config × seed), finishing one mode fully before the next. Resumable. |
| `run_arch_compare.sh <config>` | UNet3+ arm → `Models/results_arch_v3/`. |
| `run_arch_fusion.sh <config>` | **`mbfusion` arm** → `Models/results_arch_fusion_v3/`. Prints a seed-coverage table across all three arms when it finishes. |
| `run_predict_factorial.sh <config> <cluster> <huc> [seed]` | HUC inference from the best-macro-F1 seed. |
| `rsync_huc_sources.sh <cluster> <huc>` | Pull the ~7 per-HUC source rasters. |
| `run_shap_factorial.sh` | SHAP wrapper (handles the container's `HOME`/`MPLCONFIGDIR` and a missing `shap`). |
| `run_aggregate.sh` | Wraps `dl_08` → `<RESULTS_DIR>/analysis/`. |
| `rsync_results.sh` | Pull a results tree GPU→CPU. `--metrics-only` = JSON/CSV/PNG/**NPZ**/**LOG**, no weights. |
| `run_tensorboard.sh` | Serve TensorBoard over a results root from the host. |
| `run_patchcurve.sh` | **Not part of v3** (PLAN §9.2). Kept so the v2 curve reproduces. |

### 2d. Env knobs

| Var | Default | Use |
|---|---|---|
| `MODES` | `multiclass binary` | outer loop of `run_factorial.sh` |
| `MODE` | `multiclass` | single-mode drivers (`run_config.sh`, arch arms, predict) |
| `SEEDS` | `0 1 2 3 4` | replicates (R=5 in v3; `"0 1 2"` for a v2-style R=3) |
| `CONFIGS` | all 8 | subset |
| `EPOCHS` | `50` | |
| `BATCH_SIZE` | `16` (base) / `8` (arch arms) | drop to 4 on OOM |
| `BASE_FILTERS` / `DEPTH` | `64` / `5` | held constant |
| `PRECISION` | `16-mixed` | |
| `LEAKAGE_GUARD` | `huc12` | `coord` for the sensitivity run |
| `STATS_DIR` | `Data/Training_Data/stats` | |
| `RESULTS_DIR` | `Models/factorial_results_v3` | mode token is appended by `run_config.sh` |
| `ARCH` | `unet` | `unet3plus` \| `mbfusion` (set by the arm drivers) |
| `CELL_NAME` | `$CONFIG` | cell dir under `RESULTS_DIR/<mode>` |
| `GATE_KERNEL` | `3` | `[mbfusion]` gate kernel |
| `CAT_CHANNELS` / `DEEP_SUPERVISION` | `64` / `0` | `[unet3plus]` |
| `N_PATCHES` | unset | learning-curve cap (unused in v3) |
| `DRY_RUN` | `0` | `1` prints every resolved path and command, trains nothing |

---

## 3. One-time prep (CPU node) — the hard gate

From `/ibstorage/anthony/NYS_Wetlands_DL`, env active
(`conda activate wetland-cnn`, or `source .venv/bin/activate` where a venv exists):

```bash
PIPE=Python_Code_Analysis/DL_Pipeline_v2

# 1. Channel matrix self-check (source of truth)
python $PIPE/dl_experiment_config.py            # ends "All channel counts match"

# 2. Rebuild the MASTER stats  <-- REQUIRED FOR v3, see the warning below
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir  Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power 0.5
#   -> Data/Training_Data/multiclass_normalization_stats_wp0.5.json

# 3. Derive the 8 per-config stats from the master, per mode (16 files)
python $PIPE/dl_make_config_stats.py --all --mode multiclass
python $PIPE/dl_make_config_stats.py --all --mode binary

# 4. PREFLIGHT -- hard gate before any GPU time
python $PIPE/dl_preflight_check.py              # expect 0 failures
#   flags: --modes / --seeds (default 0 1 2 3 4) / --leakage-guard / --sample
#          / --data-root / --stats-dir / --norm-master
```

> ### ⚠️ The master stats are stale — step 2 is not optional for v3
>
> `multiclass_normalization_stats_wp0.5.json` was built over **1007** patches;
> `R_Patches` now holds **1012**. The patch count feeds the field class weights, so
> the master *and* all 16 per-config files are stale. Rebuild (steps 2→3), then
> re-run the preflight.
>
> Also check: `binary_normalization_stats_wp0.5.json` is currently a **26-channel
> file dated 2026-06-05** (v2 vintage). `dl_make_config_stats.py` derives *both*
> modes' normalization from the multiclass master and recomputes binary class
> weights from disk, so this file may be vestigial — confirm rather than assume.
>
> Cosmetic: `Data/Training_Data/stats/` still holds v1-vintage `fld_chm_*` files for
> a LiDAR tier that no longer exists. Nothing resolves them; they only confuse a
> reader listing the directory.

**One gotcha on step 2:** pass `--weight-power 0.5` (the default is `1.0`). The
output path follows from it — `dl_01` prints `[dl_01] writing -> <path>` before the
scan, so check that line says `..._wp0.5.json`. On success stdout also shows
`Overrode min/max with global stats for: [...]`; a `Warning: No global stats for
min_max bands: [...]` instead means the global JSON's band-name keys did not match
and the override did **not** apply.

**Stats chain — who feeds whom.** The per-config files are pure subsets of one
**master**; the master carries the normalization min/max and the class weights, and
gets its min_max ranges from the global full-raster scan in the sibling
`NYS_Wetlands_Data` project:

```
R full-raster scan (DL_Extract_Normalize_Stats_FullRasters.R, SLURM)
  → Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json
     → dl_01_compute_statistics.py --global-stats  → multiclass_normalization_stats_wp0.5.json
        → dl_make_config_stats.py --all --mode <m> → stats/<m>_..._<config>_wp0.5.json  (8 per mode)
```

Rebuild the master only when the R scan, the predictor bands, or the training
patches changed — **all three changed for v3.**

**What the preflight asserts** (all must be green for 8 configs × 2 modes):

- Location-key parity: `R_Patches` ↔ `R_Patches_NWI` paired 1:1 via
  `nwi_field_twin()`; `R_Patches_NWIextra` HUC12s ⊆ field HUC12s.
- Footprint identity per paired twin (CRS / transform / width / height / nodata).
- Predictor parity across directories against the master's `predictor_names`.
- Label values ⊆ `{0,1,2,3,255}` (binary remap → `{0,1,255}`); per-directory class
  prevalence reported so the `flddeg` target is *measured*, not assumed.
- NWI 255-mask == field 255-mask per twin.
- **The headline gate:** no `test_fld` key reaches any config's train/val pool under
  its guard.
- Per-config channels resolve and the stats file exists.
- **[9] Fusion branch partition** — branch slices are disjoint, cover all 29
  channels, and keep the one-hot block contiguous inside terrain. This is the guard
  against the fusion arm's one silent failure mode: a wrong branch map trains fine
  and reports plausible numbers while each encoder reads the wrong bands.

---

## 4. Build / load the Docker image

The repo-root `Dockerfile` builds `nys-wetlands-dl`. Build it on a machine with
Docker (e.g. your Mac), targeting amd64:

```bash
cd NYS_Wetlands_DL/
docker build --platform linux/amd64 -t nys-wetlands-dl .
docker save nys-wetlands-dl | gzip > nys-wetlands-dl.tar.gz
scp nys-wetlands-dl.tar.gz $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/
```

On the GPU node, load it with **`docker1`**, never bare `docker`:

```bash
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz
docker1 images | grep nys-wetlands-dl        # confirm
```

> The image's default `CMD` runs the single-model HPC pipeline. Every command in
> this guide **overrides** it, so the baked-in `CMD` is irrelevant here.

---

## 5. Stage repo + data onto `/workdir`

Containers may only mount under `/workdir/$USER`, so the tree has to be pushed to
the GPU node. **Use the narrow push — not `rsync -av` of the whole repo.**

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU_NODE=cbsugpu10.biohpc.cornell.edu bash Shell_Scripts/rsync_push_v3.sh -n   # preview
GPU_NODE=cbsugpu10.biohpc.cornell.edu bash Shell_Scripts/rsync_push_v3.sh      # ~14 GB
```

**Why not the whole repo.** A blanket `rsync -av --exclude .git --exclude .venv`
moves **~458 GB**, because the repo root accumulates every previous generation's
outputs: `Models/` is 276 GB of v1/v2/patchcurve/arch checkpoints and
`Data/HUC_DL_Predictions_v2/` is 154 GB of inference GeoTIFFs. **A v3 run reads
none of it** — `run_config.sh` touches only `$STATS_DIR` and the three patch
directories, and every cell it produces is created fresh under
`Models/factorial_results_v3/`. The blanket push also sweeps the 3.6 GB Docker
image tarball *into* the repo copy, where the container would see it through
`/app` for no reason.

What the script sends, and why each is load-bearing:

| Path | Size | Read by |
|---|---|---|
| `Python_Code_Analysis/DL_Pipeline_v2` | 34 MB | the whole pipeline; `dl_experiment_config.py --emit` |
| `Shell_Scripts` | <1 MB | every `run_*.sh` driver |
| `Data/Training_Data/stats` | 264 KB | `run_config.sh` → `$TRAIN_STATS` / `$EVAL_STATS` |
| `Data/Training_Data/R_Patches` | 4.5 GB | field labels — **TEST for every cell** |
| `Data/Training_Data/R_Patches_NWI` | 4.5 GB | `nwi`/`nwifield`/`flddeg` train pools |
| `Data/Training_Data/R_Patches_NWIextra` | 4.7 GB | `nwiextra`/`nwifield` train pools |
| `.git_commit` | 12 B | manifest provenance (see below) |

Deliberately left behind: all of `Models/`, both `HUC_DL_Predictions_*` roots,
`R_Patches_Merged*` (the v1/v2 pools), `stats_v1/` + `stats_v2/`, the master
`*_normalization_stats_wp0.5.json` (consumed by `dl_make_config_stats.py` on the
**CPU** node only), `.venv`, `.git`, and `__pycache__` / `.ipynb_checkpoints`.

**Provenance.** `.git` is not pushed, so `git rev-parse` on the node fails and
`run_config.sh` would stamp `git_commit: "unknown"` into all 100 manifests. The
script writes a `.git_commit` file (short SHA, `-dirty` suffix if the tree is
modified) and stages it; `run_config.sh` prefers `$GIT_COMMIT`, then that file,
then `git`. Nothing else to do — but note that a `-dirty` stamp in a manifest
means the grid ran on uncommitted code.

**The Docker image** goes to `/workdir/$USER/`, one level *above* the repo copy:

```bash
GPU_NODE=cbsugpu10.biohpc.cornell.edu bash Shell_Scripts/rsync_push_v3.sh --with-image
```

`--relative` recreates each path under the destination, so the node mirrors the
local tree and `…/nys_wetlands/X` becomes `/app/X` once mounted. **Re-run before
every reservation** even if `/workdir/$USER/nys_wetlands` survived the last one —
rsync moves only what changed, and running stale wrappers is the classic silent
failure here.

---

## 6. Launch the base grid

80 cells ≈ 27–80 GPU-h — it will outlive an SSH session, so run it under `tmux`.

```bash
# ON the GPU node:
tmux new -s factorial_v3
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh
# Detach: Ctrl-b then d   |   Reattach: tmux attach -t factorial_v3
```

### Anatomy of the `docker1` wrapper

Every "GPU node, in-container" command in this guide uses this template; only the
`-e` knobs and the trailing command change:

```bash
tmux new -s <session>
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e <KNOB>=<value> ... \
  nys-wetlands-dl \
  <the in-container command verbatim>
```

- **`docker1`** — the BioHPC privileged wrapper. Never bare `docker`.
- **`--gpus all`** — exposes the A6000.
- **`--shm-size=8g`** — DataLoader workers use shared memory; the 64 MB default
  causes `bus error` crashes.
- **`--user $(id -u):$(id -g)`** — outputs owned by you, not root.
- **`-v /workdir/$USER/nys_wetlands:/app`** — the only allowed mount; `/app` is the
  image `WORKDIR`, so scripts resolve the repo root to the mounted tree.
- **`--rm`** — the container is disposable; all state is on the mount.

> ### ⚠️ Env knobs must go through `-e`
>
> A host-side prefix (`MODE=binary docker1 run …`) sets the variable in **your
> shell**, not in the container — the script inside silently falls back to its
> defaults and you get the wrong run. One `-e NAME=value` per knob; quote lists
> (`-e SEEDS="0 1 2 3 4"`). **Paths in `-e` values are container paths** (`/app/…`),
> since the script runs inside; only the `-v` mount source is a host path.

**Verify at launch.** Every driver echoes its resolved plan (mode, seeds, arch, cell
path, stats files) in its first lines — read those before detaching. A dry run
prints all of it without training:

```bash
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e DRY_RUN=1 \
  nys-wetlands-dl bash Shell_Scripts/run_factorial.sh
```

**Run a subset:**

```bash
-e MODES="multiclass"                                        # one mode
-e CONFIGS="nwi_chmret_leafoff flddeg_chmret_leafoff"        # some configs
-e SEEDS="0 1 2"                                             # fewer replicates
# or one config wrapper directly:
nys-wetlands-dl bash Shell_Scripts/run_fld_chmret_leafoff.sh
```

The driver's outer loop is `MODES`, so a run stopped partway still yields a complete
multiclass factorial — finish one mode's grid before the other for
partial-reservation safety.

Each cell writes `Models/factorial_results_v3/<mode>/<config>/seed<k>/`:
`manifest.json` (fully self-describing: bands, `in_channels`, mode, label source,
patch dirs, pool rule, leakage regime, class weights, loss, arch, git commit, stats
files, degrade provenance), `metrics.json`, `confusion_matrix.csv`,
`training_log.json`, the best `.safetensors`/`.ckpt`, `train.log`, `tb_logs/`.

---

## 7. Monitor, resume, sync back

**Monitor** (from the GPU node, while the container runs):

```bash
ls /workdir/$USER/nys_wetlands/Models/factorial_results_v3/*/*/seed*/metrics.json | wc -l   # of 80
tail -f /workdir/$USER/nys_wetlands/Models/factorial_results_v3/<mode>/<config>/seed<k>/train.log
```

**TensorBoard** — the trainer writes event files to each cell's `tb_logs/`.
TensorBoard is only a *reader*, so it runs on the **host**, outside the training
container; pointing `--logdir` at the results root makes every cell its own run, and
new cells appear as the grid reaches them.

```bash
# GPU node, a SECOND tmux window (NOT inside the container):
cd /workdir/$USER/nys_wetlands
RESULTS_DIR=/workdir/$USER/nys_wetlands/Models/factorial_results_v3 \
  PORT=6006 Shell_Scripts/run_tensorboard.sh    # USE_SCREEN=1 to self-detach into `screen`
# From your laptop, tunnel and open http://localhost:6006
ssh -N -L 6006:cbsugpu10.biohpc.cornell.edu:6006 $USER@cbsulogin.biohpc.cornell.edu
```

The script needs `tensorboard` importable in *some* env on the node — it uses
`tensorboard` on PATH, else falls back to `uv run`. With neither, make a throwaway
venv just for the reader:

```bash
python -m venv /workdir/$USER/tb_env          # module load python/3.12.7 first if needed
source /workdir/$USER/tb_env/bin/activate
pip install setuptools tensorboard            # setuptools required: TensorBoard imports
                                              # pkg_resources, which py3.12 venvs omit
rm -rf /workdir/$USER/tb_env/lib64            # RHEL lib64->lib symlink otherwise doubles
                                              # every plugin -> "Duplicate plugins for name projector"
```

**Resume** across reservation windows: rerun the same command. Cells with
`metrics.json` + `manifest.json` are skipped; failed cells are reported in the
summary and retried next pass.

**Mid-run pull** (the full end-of-experiment checklist is §11):

```bash
# FROM the CPU node:
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v3" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v3" \
  Shell_Scripts/rsync_results.sh --metrics-only    # -n to preview; drop flag for weights
```

`--metrics-only` pulls `*.json`, `*.csv`, `*.png`, `*.npz`, and `*.log`. The `.log`
matters: Lightning's model summary in `train.log` is the **only** source of the
GFLOPs column in `arch_cost.csv`.

---

## 8. Aggregation (CPU) and SHAP (GPU)

The split mirrors the node split. **Aggregation is pure pandas** and safe on a
partial tree; **SHAP backprops through each model**, so it must run inside the
container *before* reservation teardown.

**Aggregation — CPU node, after sync-back, once per mode:**

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
RESULTS_DIR=Models/factorial_results_v3/multiclass Shell_Scripts/run_aggregate.sh
RESULTS_DIR=Models/factorial_results_v3/binary     Shell_Scripts/run_aggregate.sh
# writes <results-dir>/analysis/:
#   factorial_long.csv       per (config, seed, class) precision/recall/f1/iou
#   factorial_summary.csv    mean & sd over seeds
#   factorial_table.csv      headline pivot: FSW/UPL IoU+recall, macro-F1 (mean±sd)
#   contrasts.csv            paired-by-seed effects (LiDAR, leaf-off, interaction,
#                            label gradient)
#   confusion_mean/<config>.csv    seed-mean confusion matrix
#   coverage.csv             which (config × seed) cells are present
```

Contrasts are **paired by seed** — the same seed gives the same split across all 8
configs, so per-seed differences net out split luck before the mean±sd. The script
prints coverage (e.g. `62/80 cells`) and computes every contrast it has cells for, so
re-running as the grid fills in just extends the table.

**SHAP — GPU node, in-container, once per mode:**

```bash
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
    --results-dir Models/factorial_results_v3/multiclass
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
    --results-dir Models/factorial_results_v3/binary --mode binary
```

`dl_09` resolves each cell's **actual** pools split (same seed + guard as training →
SHAP background = the cell's train pool, SHAP test = its held-out field patches),
covers **all 8 configs** by default, and auto-globs every seed dir present. Scope
with `--configs` / `--seeds`; memory/cost knobs are `--n-background` (50),
`--n-test` (20), `--crop-size` (128). Run it under `tmux` — it is minutes per cell
across 80 cells.

Per cell it writes `shap/`:

- `*_shap_importance.json` — per-band importance in **both** aggregations:
  `importance_overall(_per_class)` = **SUM** over a band's channels;
  `..._per_channel` = **MEAN** (sum / `n_channels`)
- `*_shap_per_channel.npz` — spatially-averaged per-channel |SHAP|
  (`shap_abs (n_classes, n_test, C_input)` + channel→band map)
- `*_shap_band_importance*.png`, `*_shap_summary_plot.png`

> **Why both aggregations.** The one-hot `Geomorph_local` band is 10 channels while
> every continuous band is 1, so the SUM inflates it ~10×. The per-channel MEAN is
> the fair comparison; the truth sits between. Report both, and use the `.npz` to
> split Geomorphon into its 10 forms.

> **`--force` on any re-run.** `dl_09` skips a cell that already has a
> `*_shap_importance.json`. Without `--force`, a second pass silently does nothing.

Pair `contrasts.csv` (ablation = marginal contribution) against the SHAP JSON
(reliance) for the feature story.

---

## 9. Architecture comparison — the three arms

v3's headline addition. All three arms run **one config** (`fld_chmret_leafoff`, 29
ch), **both modes**, **the same 5 seeds**, bf64/d5/50-epochs held — so the only
variable is the network. Design in `PLAN.md` §6.

| Arm | Driver | Root |
|---|---|---|
| U-Net | the base grid itself (§6) | `Models/factorial_results_v3/<mode>/fld_chmret_leafoff/` |
| UNet3+ | `run_arch_compare.sh` | `Models/results_arch_v3/<mode>/fld_chmret_leafoff_unet3plus/` |
| `mbfusion` | `run_arch_fusion.sh` | `Models/results_arch_fusion_v3/<mode>/fld_chmret_leafoff_mbfusion/` |

> **No arm is pre-trained.** The v2 arms are 26-channel runs on the old pool and are
> **not comparable**. All three retrain on the 29-channel v3 stack.

> **Seeds must match across arms.** The paired comparison uses the *intersection*, so
> a short arm silently shrinks n rather than erroring. All three drivers default to
> `SEEDS="0 1 2 3 4"`, so a completed v3 grid already supplies the U-Net arm at the
> right seeds — no top-up run. `run_arch_fusion.sh` prints a seed-coverage table
> across all three arms when it finishes; **check it before aggregating.**

### 9.1 Run the two extra arms

Drop each into the §6 `docker1` wrapper's final slot, inside `tmux`:

```bash
# UNet3+ (deep supervision ON, cat_channels 64):
bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff
# binary: add  -e MODE=binary  to the docker1 line

# Multi-branch fusion:
bash Shell_Scripts/run_arch_fusion.sh fld_chmret_leafoff
# binary: add  -e MODE=binary
```

Worked example — the fusion arm, binary mode, all 5 seeds:

# Run them one at a time — each wants the whole GPU. 
```bash
  cd /workdir/$USER/nys_wetlands                                                                                                                                        
  tmux new -s arch_v3     # Ctrl-b d to detach                                                                                                                          
                                                                                                                                                                        
  1 · UNet3+ multiclass                                                                                                                                                 
  docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \                                                                                                  
    -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \                                                                                                            
    nys-wetlands-dl \                                                                                                                                                   
    bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff                                                                                                           
                                                                                                                                                                        
  2 · UNet3+ binary                                                                                                                                                     
  docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \                                                                                                  
    -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \                                                                                                            
    -e MODE=binary \                                                                                                                                                    
    nys-wetlands-dl \                                                                                                                                                   
    bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff                                                                                                           
                                                                                                                                                                        
  3 · mbfusion multiclass                                                                                                                                               
  docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \                                                                                                  
    -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \                                                                                                            
    nys-wetlands-dl \                                                                                                                                                   
    bash Shell_Scripts/run_arch_fusion.sh fld_chmret_leafoff                                                                                                            
                                                                                                                                                                        
  4 · mbfusion binary                                                                                                                                                   
  docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \                                                                                                  
    -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \                                                                                                            
    -e MODE=binary \                                                                                                                                                    
    nys-wetlands-dl \                                                                                                                                                   
    bash Shell_Scripts/run_arch_fusion.sh fld_chmret_leafoff                                                                                                            
                                                                                                                                                                        
  
```

**Memory.** Both arms default to `BATCH_SIZE=8`. For `mbfusion`, params are ~1.3×
the U-Net (162 M vs 125 M at bf64/d5/29ch), but the binding constraint is
*activations*: at level 0 the fused tensor is 144 channels at 256² against the
U-Net's 64 — **2.25× the finest-scale activation**. Expect one OOM-and-halve
iteration on first launch. Still lighter than UNet3+.

**The branch map is not a knob.** The trainer derives it from the config's stats file
(`stats["predictor_names"]`, in post-one-hot-expansion channel space) and stores it
in the checkpoint + `.meta.json`, so eval/predict auto-detect it and a
`nolidar`/`leafon` config simply yields fewer branches. `dl_preflight_check` **[9]**
gates it on CPU before any GPU time — a wrong map trains fine and reports plausible
numbers while each encoder reads the wrong bands.

**Watch for gate collapse.** TensorBoard scalars `train/gate_entropy/level0..5`.
Healthy is near `log(n_branch)` = 1.386 for four branches; trending toward 0 in the
first few epochs means the gate collapsed onto one branch. The standard fix is a
temperature on the gate logits — deliberately not built in speculatively.

### 9.2 Export gate rasters (a deliverable, not a debug artifact)

Run before teardown, per cell you want maps for:

```bash
python $PIPE/dl_11_export_gates.py \
  --cell Models/results_arch_fusion_v3/multiclass/fld_chmret_leafoff_mbfusion/seed0 \
  --config fld_chmret_leafoff --seed 0 --mode multiclass
```

Writes `<cell>/gates/<patch>.npz` (six float16 `(n_branch, H, W)` arrays, ~0.5 MB per
patch) + `gate_summary.json`, from a deterministic prefix of the seed's held-out
field patches (`--n-patches`, default 8). `rsync_results.sh --metrics-only` includes
`*.npz`, so they come back with the JSON/CSV.

> **Reading gate maps — the one caveat.** After gating, `proj` is a 1×1 conv, so the
> decoder sees `Σᵢ Wᵢ(fᵢ·gᵢ)`. **Valid:** within-branch spatial comparison ("terrain
> reliance rises in depressions relative to sideslopes") — the gate is the only thing
> varying across space. **Confounded:** cross-branch absolute comparison ("terrain
> matters more than LiDAR overall"), since a branch with modest gates but large `Wᵢ`
> can still dominate. GroupNorm equalizes features, not projection weights. Plot
> gates **standardized within branch**, and take overall branch importance from
> **SHAP**. The means in `gate_summary.json` are provenance, not a ranking.

### 9.3 Aggregate the three arms (CPU, once per mode)

```bash
python $PIPE/dl_08b_aggregate_patchcurve.py --arch-compare \
  --config fld_chmret_leafoff --mode multiclass \
  --arch-dir unet=Models/factorial_results_v3 \
  --arch-dir unet3plus=Models/results_arch_v3 \
  --arch-dir mbfusion=Models/results_arch_fusion_v3
```

`--mode` is appended to each root when the root does not already end in it, so both
path forms work. The cell inside a root is `<config>_<name>`, falling back to plain
`<config>` for the base grid. Four CSVs land in the **last** arm's `<root>/<mode>/analysis/`
(override with `--output-dir`):

| File | What it holds |
|---|---|
| `arch_compare_long.csv` | one row per (arch, seed) — every metric plus cost. The tidy form; plot from this. |
| `arch_contrasts.csv` | paired per-seed deltas vs the baseline arm (first `--arch-dir`, or `--baseline`), with `n_better`/`n_seeds` |
| `arch_cost.csv` | params, GFLOPs, sec/epoch per arm, and params as a multiple of the baseline's |
| `arch_compare.csv` | wide per-seed table + seed-mean row (v2-shaped) |

Two things to know. **`--confusion-pair` (default `FSW UPL`)** adds row-normalized
directional confusion rates — the share of true-FSW pixels predicted UPL and vice
versa — which is the specific failure the fusion encoder targets; in binary mode
neither class exists, so those rows are simply absent. And **at n=5 the credible
summary is sign consistency** (`n_better`/`n_seeds`), not a p-value: same seed ⇒ same
test patches ⇒ each delta is genuinely paired, but five paired differences do not
support a distributional claim. No p-values are computed, deliberately.

`--unet-dir` / `--unet3plus-dir` still work as deprecated two-arm aliases, so the v2
output reproduces from the same script.

---

## 10. HUC prediction / inference maps

Per-HUC class raster + per-class softmax probabilities. Unlike training, this needs
the **source rasters** for the target HUC, which live **outside** the `/app` mount —
so this study uses a **two-mount** container, not the §6 wrapper.

Prediction needs **no training patches**: `dl_06b_predict_huc.py` assembles the stack
in-memory. Per config it needs only the code, the mode-tokened `stats/`, and each
cell's `best_*.safetensors` + `metrics.json` (seed selection) + `manifest.json`
(arch/bf/depth) — ~50 MB per cell with `.ckpt` excluded:

```bash
# 1. FROM the CPU node -- restage just what prediction needs:
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU_NODE=cbsugpu10.biohpc.cornell.edu
rsync -avhP --relative \
  --exclude='*.ckpt' --exclude='__pycache__' \
  --exclude='tb_logs' --exclude='lightning_logs' --exclude='shap' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts \
  Data/Training_Data/stats \
  Models/factorial_results_v3/multiclass/fld_chmret_leafoff \
  Models/factorial_results_v3/binary/fld_chmret_leafoff \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"

# 2. Pull the ~7 per-HUC source tiles (~4-5 GB/HUC), not the ~1.74 TB tree:
SERVER="$USER@$DATA_HOST:" REMOTE_ROOT=/ibstorage/anthony/NYS_Wetlands_Data \
LOCAL_ROOT=/workdir/$USER/NYS_Wetlands_Data \
  bash Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>      # -n to preview

# 3. Verify the band/channel contract before predicting (no model needed):
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -v /workdir/$USER/NYS_Wetlands_Data:/data \
  nys-wetlands-dl \
  python Python_Code_Analysis/DL_Pipeline_v2/dl_huc_stack.py \
    --huc <huc> --cluster <cluster> --data-root /data --inspect

# 4. Predict -- same two mounts, plus DATA_ROOT:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -v /workdir/$USER/NYS_Wetlands_Data:/data \
  -e DATA_ROOT=/data -e TMPDIR=/app/tmp \
  nys-wetlands-dl \
  bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>
#   add -e MODE=binary for the binary model; -e DRY_RUN=1 to resolve without predicting
```

Output: `Data/HUC_DL_Predictions_v3/DLpred_<mode>_cluster_<C>_huc_<H>.tif` (class)
and `..._probs.tif` (per-class softmax). `RESULTS_DIR` defaults to
`Models/factorial_results_v3/<MODE>`, falling back to `_v2`, then the v1 root, then
`results/`. Append a seed argument to pin one; otherwise the best-macro-F1 seed is
chosen. Architecture is auto-detected from the checkpoint, so UNet3+ and `mbfusion`
cells work unchanged.

**Batch over many HUCs:** list them in `Shell_Scripts/huc.txt`, one
`<cluster>:<huc>` per line (blank lines ignored), then loop inside a single two-mount
container under `tmux`:

```bash
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -v /workdir/$USER/NYS_Wetlands_Data:/data \
  -e DATA_ROOT=/data -e TMPDIR=/app/tmp \
  nys-wetlands-dl bash -c '
    while IFS=: read -r cluster huc; do
      [[ -z "$cluster" || -z "$huc" ]] && continue
      bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff "$cluster" "$huc" \
        || echo "[FAILED] cluster $cluster huc $huc"
    done < Shell_Scripts/huc.txt'
```

The `|| echo [FAILED]` keeps the loop going past a HUC with missing source tiles —
grep the scrollback for `FAILED` afterwards. Source rasters for every listed HUC must
already be pulled. Start with 2–3 demo HUCs to validate before scaling up.

---

## 11. End-of-reservation sync-back (everything)

Everything below runs **FROM the CPU node**. `/workdir` on the GPU node is local,
unshared, and not backed up — treat this as the checklist to drain a reservation
before teardown. All passes are idempotent: safe mid-training and safe to re-run.

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU="$USER@cbsugpu09.biohpc.cornell.edu"     # whichever node held the reservation
WD="/workdir/$USER/nys_wetlands"
```

**1 · Base grid** (both modes ride along — the root is mode-tokened; SHAP JSON/PNG
live inside the cells, so they come with `--metrics-only`):

```bash
SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/factorial_results_v3" \
LOCAL_DEST=Models/factorial_results_v3 \
  Shell_Scripts/rsync_results.sh --metrics-only        # -n first to preview
```

**2 · Architecture arms** (same script, different roots):

```bash
for tree in results_arch_v3 results_arch_fusion_v3; do
  SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/$tree" LOCAL_DEST="Models/$tree" \
    Shell_Scripts/rsync_results.sh --metrics-only
done
```

**3 · Model weights** (once, before teardown). `--metrics-only` excludes
`.ckpt`/`.safetensors`; rerun 1–2 **without** the flag — already-synced metrics are
skipped, only weights move. Keep just the safetensors (~50 MB/cell vs ~500 MB) with:

```bash
SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/factorial_results_v3" \
LOCAL_DEST=Models/factorial_results_v3 \
RSYNC_OPTS="-avz --progress --exclude=*.ckpt" \
  Shell_Scripts/rsync_results.sh
# repeat per arch tree if those weights are wanted
```

**4 · HUC prediction GeoTIFFs** (plain rsync — flat dir, no config/seed layout;
multi-GB and gitignored):

```bash
rsync -avhP "$GPU:$WD/Data/HUC_DL_Predictions_v3/" Data/HUC_DL_Predictions_v3/
```

**5 · Close the loop on CPU:** aggregate (§8 per mode) → three-arm aggregation (§9.3
per mode) → rerun the viz notebooks (§12) → `git add -A` so the whitelisted analysis
JSON/CSV sync to the local Mac via git.

---

## 12. Analysis and figures

**`R_Code_Analysis/dl_10_Factorial_viz_R.qmd` is the active viz notebook** (R/4.4.3).
Its architecture section reads `arch_compare_long.csv` and is arm-count-agnostic —
point `arch_dir_base` at the fusion root and it renders macro-F1, per-class F1, the
FSW↔UPL confusion panel, the contrast and cost tables, and the gate-weight-by-scale
plot. A fourth arm needs only an entry in `arch_name`/`arch_color`.

The Python notebooks are the older siblings: `dl_10_factorial_viz.ipynb` (§1–§5 base
factorial + SHAP, §6–§7 follow-ons; has a `MODE` selector and reads only small
CSV/JSON, which `.gitignore` whitelists so it syncs to a Mac via `git pull`) and
`dl_10b_huc_inference_viz.ipynb` (§8 HUC prevalence, reads the multi-GB prediction
GeoTIFFs directly — rsync them first). Run them on an env with rasterio + seaborn
(the conda `wetland-cnn` or uv `nys-wetlands-dl (uv)` kernel).

> ### Two schema facts any reader of a v2/v3 `metrics.json` must respect
>
> **Gotcha 1 — nested scores.** `run_config.sh` writes the score block **nested under
> `"test_metrics"`** (`overall_accuracy`/`mean_iou`/`macro_f1`/`per_class`); v1's
> `dl_05` wrote those flat at top level. Unwrap with
> `scores = metrics.get("test_metrics") or metrics`. v2/v3 also drop
> `macro_recall`/`macro_precision` from that block — recover them as the unweighted
> class mean of `per_class`, matching how `macro_f1` is defined.
>
> **Gotcha 2 — confusion matrix is a dict.** v2/v3 store `confusion_matrix` as
> `{"labels": [...], "matrix": [[...]]}` at the **top level** (not nested), where v1
> stored a bare nested list. `np.array(cm)` on the dict raises. Parse with: if
> `isinstance(cm, dict)` use `cm["matrix"]` + `cm["labels"]`, else the bare list with
> labels from `class_names`/`per_class`.
>
> `dl_08` and `dl_08b` both handle these already; the note is for anyone writing new
> analysis code.

**Cross-mode comparison** (`analysis/cross_mode_summary.csv`): macro-F1 is
deliberately **not** compared across modes (different class counts). The two
apples-to-apples views are **UPL** (identical class in both modes → mean ± sd over
seeds) and **WET** (collapse each multiclass model's seed-mean confusion matrix
EMW/FSW/SSW→WET and compare against the native-binary WET — the fair "collapse a
4-class model vs. train binary" baseline), plus the label-gradient panel
(`nwi → nwiextra → nwifield → flddeg → fld`) in both modes.

---

## 13. Pre-launch checklist

- [ ] Patch dirs current: `R_Patches`, `R_Patches_NWI`, `R_Patches_NWIextra`
- [ ] `python $PIPE/dl_experiment_config.py` self-check passes (21/25/29)
- [ ] **Master stats rebuilt** over the current patch count (§3 — currently stale:
      master says 1007, disk has 1012)
- [ ] `dl_make_config_stats.py --all` run for **both** modes (16 files)
- [ ] `dl_preflight_check.py` GREEN for 8 configs × 2 modes — including the leakage
      guard and **[9]** the fusion branch partition
- [ ] Repo + 3 patch dirs rsynced to `/workdir/$USER/nys_wetlands`; image loaded
      (`docker1 images`)
- [ ] `DRY_RUN=1` confirms cells target `Models/factorial_results_v3/<mode>/…`
      before any GPU time
- [ ] Driver launched inside `tmux`; mount only under `/workdir/$USER`; knobs passed
      via `-e`
- [ ] `rsync_results.sh -n` dry-run round-trips before the first real pull
- [ ] Heavy jobs run by **the user** — the agent does not auto-execute

---

## 14. Versioning ritual — how v3 stays separate from v2

**One convention drives it:** every output root carries the version, and every root
is an env knob or a CLI flag. v3 is already wired this way — `run_config.sh` defaults
`RESULTS_DIR` to `Models/factorial_results_v3`, the arm drivers default to
`results_arch_v3` / `results_arch_fusion_v3`, and `run_predict_factorial.sh` writes
`Data/HUC_DL_Predictions_v3`. **You do not need to export anything for a normal v3
run.** This section is for the *next* version bump.

### 14.1 Why a naïve repeat is unsafe

Three classes of artifact overwrite **in place**, and `Models/` + `Data/` are
gitignored — so a git tag preserves *code and docs* but **not** results, stats, or
patches:

| Artifact | Fixed path | What a naïve repeat does |
|---|---|---|
| Trained cells | `Models/factorial_results_<v>/<mode>/<config>/seed<k>/` | The idempotent skip sees the old `metrics.json` and **skips every cell** — old results masquerade as a fresh run. Deleting to force destroys them. |
| Per-config stats | `Data/Training_Data/stats/*_<config>_wp0.5.json` | `dl_make_config_stats --all` overwrites them, and the master too. Old metrics survive, but old **SHAP / predict** (which reload stats) can no longer be reproduced. |
| Patches | `Data/Training_Data/R_Patches*/` | If predictors changed, the band schema changes and old checkpoints no longer load against them. |
| HUC predictions | `Data/HUC_DL_Predictions_<v>/DLpred_...tif` | Filename has no version token — same HUC overwrites. |
| Viz notebooks | hardcoded roots | Re-running repoints at whatever is on disk and overwrites rendered figures. |

### 14.2 The ritual

**Step A — freeze code + docs with a git tag** (`git tag factorial-v2`). Tags are
immutable and need no maintenance. **Never** use a long-lived branch for this: the
workstreams here share the entire core, so a parallel branch means every core fix is
applied twice and reconciled at merge — and this repo's merges are `.ipynb`-JSON and
`.gitignore` merges, the painful kind.

**Step B — snapshot the gitignored artifacts** (copy, do not move — prep regenerates
the canonical paths):

```bash
cp -a Models/factorial_results_v2           Models/factorial_results_v2_frozen
cp -a Data/Training_Data/stats              Data/Training_Data/stats_v2
cp -a Data/Training_Data/multiclass_normalization_stats_wp0.5.json \
      Data/Training_Data/multiclass_normalization_stats_wp0.5_v2.json
```

(This was done for v1 and v2 — `stats_v1`, `stats_v2`,
`multiclass_normalization_stats_wp0.5_v{1,2}.json`, and `R_Patches_Merged_v1` are all
on disk today.)

**Step C — bump the roots.** For a schema change (new predictor bands), edit in **one
commit** before prep so the preflight stays the guardrail:

1. `dl_experiment_config.py` — add the band to `BASE_BANDS`/`LIDAR_TIERS`/
   `SPECTRAL_TIERS`, **and** update the 8 `"channels"` values, or
   `verify_channel_matrix()` (and thus the preflight) fails. If the band belongs to a
   fusion branch, add it to `BRANCH_BANDS` too, or preflight **[9]** fails.
2. `dl_band_config.json` — add the new band's normalization rule.
3. Rebuild the global raster scan in the sibling `NYS_Wetlands_Data` project so the
   new band has global min/max.
4. Update the channel tables in `PLAN.md` §3.5 and §2a here.
5. Bump the default `RESULTS_DIR` in `run_config.sh` and the arm drivers, and
   `OUT_DIR` in `run_predict_factorial.sh`.
6. Then run the §3 prep block and the preflight.

> **`in_channels` changing means old checkpoints are incompatible** — no warm-start;
> the new version is a full retrain. That is expected, and is exactly why the old
> version was snapshotted. (This is what happened at v2→v3.)

**Step D — confirm before burning GPU time.** `DRY_RUN=1` on the driver, and check
that the resolved cell paths carry the new version token.

---

## Pointers

- **Flow map:** §0 (the one table to read first)
- **Design / rationale / settled decisions:** [`PLAN.md`](PLAN.md)
- **Source of truth for the matrix:** `../dl_experiment_config.py`
- **Draining a reservation:** §11
- **Schema gotchas for new analysis code:** §12
- **Superseded runbooks (v1, v2) and the original fusion plan:** `archive/`
- **Production model (separate project):** `../production_model/`
