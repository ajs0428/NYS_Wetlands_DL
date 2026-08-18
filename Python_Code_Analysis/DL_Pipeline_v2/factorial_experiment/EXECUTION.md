# Wetland Factorial Experiment — Execution Guide

How to actually *run* the factorial experiment, end to end, on the BioHPC GPU
nodes (`cbsugpu09` / `cbsugpu10`) via `docker1`. The **design** lives in
[`wetland_factorial_experiment_plan.md`](wetland_factorial_experiment_plan.md);
this file is the operational walkthrough.

> **Agent boundary (`AGENTS.md`).** Claude Code *prepares* these scripts; **you
> run them.** Nothing here auto-launches training, containers, or long jobs.

---

## 0. TL;DR

```bash
# --- on the CPU/login node (one-time prep) -------------------------------
cd /ibstorage/anthony/NYS_Wetlands_DL
python Python_Code_Analysis/DL_Pipeline_v2/dl_experiment_config.py      # sanity: channel matrix
python Python_Code_Analysis/DL_Pipeline_v2/dl_make_config_stats.py --all  # 8 per-config stats (if not present)
python Python_Code_Analysis/DL_Pipeline_v2/dl_preflight_check.py            # must be green

# --- stage repo + data onto the GPU node's local /workdir (rsync over ssh) ---
# Run FROM the CPU node. The GPU node is NOT directly connected to the CPU
# nodes (no shared mount), so push the tree across with rsync over ssh:
rsync -av --exclude '.git' --exclude '.venv' \
      /ibstorage/anthony/NYS_Wetlands_DL/ \
      $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/

# --- load the image, launch the factorial inside tmux (ON the GPU node) ---
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz   # first time only
tmux new -s factorial
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh
# Ctrl-b then d to detach; reattach later with: tmux attach -t factorial

# --- pull results back to /ibstorage for analysis ------------------------
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v3" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v3" \
  Shell_Scripts/rsync_results.sh --metrics-only   # add no flag for full weights
```

The runner is **idempotent** — stop it at the end of a reservation, rerun the
same command next session, and it skips every completed cell.

---

## 1. Where work happens (two-node split)

| Node | Role | What runs there |
|---|---|---|
| **CPU / login node** (`/ibstorage` is the source of truth) | prep + analysis | per-config stats, `dl_preflight_check.py`, Phase 3 aggregation/plots |
| **GPU node** `cbsugpu09` / `cbsugpu10` (1× RTX A6000, 48 GB; local 7 TB `/workdir`) | training + eval + SHAP | `run_factorial.sh` inside `docker1`, inside `tmux` |

The GPU node is **not directly connected** to the CPU nodes — `/ibstorage` is
not mounted there, and `/workdir` is **local to each GPU node**, so files written
during a reservation do not appear on the CPU node by themselves. The two ways to
bridge that gap are (a) mounting storage onto the GPU node or (b) `rsync` over
ssh. **Use rsync** — a given storage volume can only be mounted in one place on
the node, so rsync is the more flexible and less fragile path. That is why there
is an explicit `rsync` out (Section 5) and an explicit `rsync_results.sh` back
(Section 7). `/ibstorage/anthony/NYS_Wetlands_DL` stays the canonical copy; push
to `/workdir` per reservation, pull results back after.

---

## 2. Script naming — the map

Two layers of files. **`dl_*` Python** = pipeline mechanism (built once,
imported). **`run_*.sh` shell** (in `Shell_Scripts/`) = orchestration you
invoke. The name of every shell wrapper *is* a config name *is* a `results/`
folder — that 1:1:1 mapping is the whole naming scheme.

### 2a. The config-name grammar: `<label>_<lidar>_<spectral>`

Every config name decodes into three experiment axes (defined once in
`dl_experiment_config.py`, the single source of truth):

| Slot | Values | Meaning |
|---|---|---|
| `<label>` | `fld` / `nwi` / `nwiextra` / `nwifield` / `flddeg` | training **label source**: field-verified / NWI paired / NWI + extra footprints / field ∪ NWI-extra / field degraded to NWI prevalence |
| `<lidar>` | `nolidar` / `chmret` | LiDAR tier: none / CHM + return-fraction bands (the v1 CHM-only tier was dropped) |
| `<spectral>` | `leafon` / `leafoff` | leaf-on NAIP only / + leaf-off NAIP RGB+NIR |

So `fld_chmret_leafoff` = field labels, full LiDAR, both seasons (the full
29-channel feature set); `nwi_chmret_leafoff` = same features, NWI labels.

The **8 configs** (feature ablation runs on field labels only; the label
comparison runs on the full feature set only — see plan §2). **v3 channel counts**
— three terrain metrics (`TPI_local`, `meanc_local`, `dmv_local`) were added
upstream, so every config gained 3 channels over v2's 18/22/26:

| Config | `in_channels` (v3) | was (v2) |
|---|---|---|
| `fld_nolidar_leafon`      | 21 | 18 |
| `fld_nolidar_leafoff`     | 25 | 22 |
| `fld_chmret_leafon`       | 25 | 22 |
| `fld_chmret_leafoff`      | 29 | 26 |
| `nwi_chmret_leafoff`      | 29 | 26 |
| `nwiextra_chmret_leafoff` | 29 | 26 |
| `nwifield_chmret_leafoff` | 29 | 26 |
| `flddeg_chmret_leafoff`   | 29 | 26 |

× 2 modes (multiclass, binary) × 5 seeds (0–4) = **80 cells**. v3 raised the
replicate count from v2's R=3 so the factorial and the three-arm architecture
comparison share one seed set. Always confirm
against `python dl_experiment_config.py` — that file, not this table, is the
source of truth.

### 2b. Python mechanism scripts (`Python_Code_Analysis/DL_Pipeline_v2/`)

| File | Role in the experiment |
|---|---|
| `dl_experiment_config.py` | **Single source of truth.** Band matrix + the 8 configs. `--list` prints config names; `--emit <config>` prints shell vars (`TRAIN_STATS`, `EVAL_STATS`, `IN_CHANNELS`, …) consumed by `run_config.sh`; no args runs the channel-matrix self-check. |
| `dl_merge_nwi_labels.py` | Phase 0: writes `MOD_CLASS_NWI` band into each patch (already done → `merge_manifest.json`). |
| `dl_degrade_labels.py` | Phase 1.3: writes `MOD_CLASS_FLDDEG` (field wetland→UPL down to NWI prevalence, seeded). Already done → `degrade_manifest.json`. |
| `dl_make_config_stats.py` | Phase 1.4: subsets the master stats into the 8 per-config stats files. `--all` writes all of them. |
| `dl_preflight_check.py` | Phase 0 gate: directory counts, predictor parity, field↔NWI pairing + footprint identity, label-value sanity, split/leakage, per-config channels + stats files, and **[9]** the fusion branch partition. Flags: `--modes` / `--seeds` (default `0 1 2 3 4`) / `--leakage-guard` / `--sample`. **Must be green before any GPU time.** |
| `dl_04_train_lightning.py` | Training (called by the runner). |
| `dl_05_evaluate.py` | Test-set metrics + confusion matrix (called by the runner; auto-detects arch from the checkpoint). |
| `dl_08_aggregate_factorial.py` | **Phase 3 aggregation (CPU).** Walks `results/<config>/seed*/metrics.json` into the factorial table + paired-by-seed contrasts (LiDAR tiers, leaf-off main effect, LiDAR×leaf-off interaction, label gradient). Pure pandas; safe to run on a partial tree (reports coverage). |
| `dl_11_export_gates.py` | **[mbfusion] Gate rasters (§10.4).** Loads a fusion cell's checkpoint and exports per-scale, per-branch softmax gates for a deterministic prefix of the seed's held-out field patches → `<cell>/gates/*.npz` + `gate_summary.json`. Standalone (does not touch `dl_05`), so it re-runs against any archived cell. |
| `dl_09_shap_factorial.py` | **Phase 3 SHAP (GPU).** Thin wrapper over `dl_07_shap_analysis.run_shap` that walks the field-trained cells, loading each cell's checkpoint + per-config stats (correct band subset) at the cell's seed → `results/<config>/seed<k>/shap/`. Idempotent; **run inside the container before reservation teardown.** |

### 2c. Shell orchestration (`Shell_Scripts/`)

| Script | What it does |
|---|---|
| `run_config.sh <config> <seed>` | **The workhorse.** Runs *one* cell: resolves stats via `dl_experiment_config.py --emit`, trains, finds the best checkpoint, evaluates on the **field** test set, writes `results/<config>/seed<k>/` (metrics, confusion matrix, manifest, logs). **Idempotent** — skips a cell whose `metrics.json` + `manifest.json` already exist. |
| `run_<config>.sh` (8 of them) | Thin wrapper: loops one config over `SEEDS` (default `0 1 2 3 4`), deferring to `run_config.sh`. Use to run a single config's replicates. |
| `run_factorial.sh` | **Top-level driver.** Walks every (config × seed) cell, seed-outer, calling `run_config.sh`; skip-completed makes it resumable. This is what you launch in `tmux`. |
| `run_arch_compare.sh <config>` | UNet3+ arm of the architecture comparison → `Models/results_arch_v3/`. |
| `run_arch_fusion.sh <config>` | **Multi-branch fusion arm** (`--arch mbfusion`) → `Models/results_arch_fusion_v3/`. See §10.4. |
| `rsync_results.sh` | Pull `results/` from the GPU node's `/workdir` back to the CPU node (`--metrics-only` for fast JSON/CSV/PNG/**NPZ**, no flag for full checkpoints). |
| `run_aggregate.sh` | **Phase 3 aggregation.** Wraps `dl_08_aggregate_factorial.py` (CPU/pandas) → `<RESULTS_DIR>/analysis/`. Defaults `RESULTS_DIR` to `Models/factorial_results` (the synced location) if present, else `results/`. Safe on a partial tree. |
| `run_tensorboard.sh` | Serve TensorBoard over `results/` from the host (one dashboard, every cell a run). |

**Key behavior to remember:** a `nwi`/`flddeg` config *trains* on its own label
source but is *evaluated* against **field** labels (plan §3, non-negotiable). The
runner enforces this automatically by evaluating with the matching
`fld_<lidar>_<spectral>` stats — same seed ⇒ same test patches.

### 2d. Useful env knobs (override on the command line)

`run_config.sh` / `run_factorial.sh` read these from the environment:

| Var | Default | Use |
|---|---|---|
| `SEEDS` | `0 1 2 3 4` | replicate seeds (R=5 in v3; `SEEDS="0 1 2"` for a v2-style R=3 run) |
| `CONFIGS` | all 8 | subset, e.g. `CONFIGS="fld_chmret_leafoff nwi_chmret_leafoff"` |
| `EPOCHS` | `50` | training epochs |
| `BATCH_SIZE` | `16` | per-step batch |
| `BASE_FILTERS` / `DEPTH` | `64` / `5` | U-Net size (HPC settings) |
| `PRECISION` | `16-mixed` | AMP precision |
| `PATCHES_DIR` | `Data/Training_Data/R_Patches_Merged` | training patches |
| `RESULTS_DIR` | `<repo>/results` | output root |
| `DRY_RUN` | `0` | `1` prints the commands without training/touching the tree |

---

## 3. One-time prep (CPU node)

From `/ibstorage/anthony/NYS_Wetlands_DL` (env active: `source .venv/bin/activate`
or `conda activate wetland-cnn`):

```bash
PIPE=Python_Code_Analysis/DL_Pipeline_v2

# 0. (Re)build the merged training patches -- ONLY when R_Patches / R_Patches_NWI
#    changed (e.g. new patches added). Skip if R_Patches_Merged is already current.
python $PIPE/dl_merge_nwi_labels.py --dry-run   # verify every field<->NWI pair; writes nothing
python $PIPE/dl_merge_nwi_labels.py             # -> R_Patches_Merged (MOD_CLASS + MOD_CLASS_NWI, 19 bands)
python $PIPE/dl_degrade_labels.py --seed 0      # adds MOD_CLASS_FLDDEG (target measured from the NWI band)

# 1. Confirm the config matrix is self-consistent (channel counts match the plan)
python $PIPE/dl_experiment_config.py            # prints the 8 configs; ends "All channel counts match"

# 2. Build the MASTER stats (only when the master is stale -- see below)
#    Output path is derived from the mode + --weight-power, so this writes
#    Data/Training_Data/multiclass_normalization_stats_wp0.5.json
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir   Data/Training_Data/R_Patches \
  --global-stats  Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power  0.5

# 3. Derive the 8 per-config stats files from the master (no raster rescan)
python $PIPE/dl_make_config_stats.py --all      # -> Data/Training_Data/stats/*.json

# 4. Preflight — HARD GATE before any GPU time
python $PIPE/dl_preflight_check.py            # expect 0 failures (warnings are advisory)
```

**Stats chain (who feeds whom).** The per-config files are pure subsets of one
**master** stats file; the master is what carries the normalization min/max and
the field class weights. The master in turn gets its min_max ranges from the
**global full-raster stats** computed in the sibling project
(`NYS_Wetlands_Data/.../DL_Extract_Normalize_Stats_FullRasters.R` via
`dl_extract_normalize_stats.sh`, SLURM) →
`Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json`:

```
R full-raster scan → HUC_DL_Stacks_Extracted_Values.json
     → dl_01_compute_statistics.py --global-stats  → multiclass_normalization_stats_wp0.5.json  (master)
          → dl_make_config_stats.py --all          → stats/..._<config>_wp0.5.json  (the 8)
```

**Data build (step 0).** The configs train from `R_Patches_Merged/` — one file
per patch carrying all three label bands (`MOD_CLASS` field, `MOD_CLASS_NWI`,
`MOD_CLASS_FLDDEG`). It is built from the two source patch dirs:

```
R_Patches/<name>.tif  +  R_Patches_NWI/NWI_<name>.tif
     → dl_merge_nwi_labels.py   → R_Patches_Merged/<name>.tif  (+ MOD_CLASS_NWI, 19 bands)
          → dl_degrade_labels.py --seed 0  → (+ MOD_CLASS_FLDDEG, 20 bands)
```

Run step 0 **whenever `R_Patches` or `R_Patches_NWI` change** (e.g. new patches
added to both). The merge pairs by name but **geometry is the trust anchor** —
every field↔NWI pair is CRS/transform/size/mask verified and ANY failed pair
aborts the run before writing, so each new `R_Patches/<name>.tif` must have a
matching `R_Patches_NWI/NWI_<name>.tif`. The merge overwrites each merged file
with a fresh 19-band copy (dropping any old `MOD_CLASS_FLDDEG`), so degrade runs
right after to re-add it; `--overwrite` on degrade is only needed if you
re-degrade *without* re-merging first. **A changed patch set makes the master and
all per-config stats stale → after step 0 you must redo steps 2–4** (the patch
count feeds field class weights, so it is not optional).

**When to run step 2 (master rebuild).** Only when the master is stale — i.e. you
re-ran the R full-raster scan, changed the predictor band set / band scaling, or
changed the training patches. Otherwise the existing master already has the
global min/max baked in (its `min_max` bands read
`"note": "Maps to [0, 1] (global raster min/max)"`), and you start at step 3.

> One gotcha on step 2: pass `--weight-power 0.5` (the default is `1.0`). The
> output path follows from it — `dl_01` prints `[dl_01] writing -> <path>` before
> the scan, so check that line says `..._wp0.5.json`. On success stdout also shows
> `Overrode min/max with global stats for: [...]`; a `Warning: No global stats for
> min_max bands: [...]` instead means the global JSON's band-name keys did not
> match and the override did **not** apply.
>
> *(Fixed 2026-08-17: `--output` used to be mandatory here, because its default was
> resolved before `--weight-power` was parsed and a `--weight-power 0.5` run would
> silently write the un-suffixed file while leaving the real master stale. It is now
> resolved after parsing. Passing `--output` still works and still overrides.)*

> As of the last session steps 1, 3, 4 were satisfied (554 merged 20-band
> patches, 8 stats files, preflight green) on the prior master. Re-run step 2
> first whenever the global stats / bands / patches changed.

---

## 4. Build / load the Docker image

The `Dockerfile` (repo root) builds `nys-wetlands-dl` with all deps. Build it on
a machine with Docker (e.g. your Mac), targeting amd64:

```bash
cd NYS_Wetlands_DL/
docker build --platform linux/amd64 -t nys-wetlands-dl .
docker save nys-wetlands-dl | gzip > nys-wetlands-dl.tar.gz
scp nys-wetlands-dl.tar.gz $USER@cbsugpu09.biohpc.cornell.edu:/workdir/$USER/
```

On the GPU node, load it (use **`docker1`**, never bare `docker` — it supplies
the BioHPC privileges):

```bash
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz
docker1 images | grep nys-wetlands-dl   # confirm
```

> The image's default `CMD` runs the *single*-model HPC pipeline. For the
> factorial we **override** it with `bash Shell_Scripts/run_factorial.sh`
> (Section 6), so the baked-in CMD is irrelevant here.

---

## 5. Stage repo + data onto `/workdir` (GPU node)

Containers may only mount paths under `/workdir/$USER` (hard BioHPC rule), and
the factorial writes `results/` *and* reads edited orchestration code + the
per-config `stats/` — so mount the **whole repo**, not just `Data/`/`Models/`.

Because the GPU node has no shared mount to the CPU nodes (Section 1), stage the
repo with **rsync over ssh**, pushing from the CPU node where `/ibstorage` lives:

```bash
# FROM the CPU node (push across the network to the GPU node's local /workdir):
ssh $USER@cbsugpu10.biohpc.cornell.edu 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -av --exclude '.git' --exclude '.venv' \
      /ibstorage/anthony/NYS_Wetlands_DL/ \
      $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/
```

Re-run the same `rsync` before a later reservation to push any code/stats edits;
it only transfers what changed. (Mounting `/ibstorage` onto the GPU node instead
is possible but a volume can be mounted in only one place per node, so rsync is
preferred.)

This brings `Python_Code_Analysis/`, `Shell_Scripts/`, and
`Data/Training_Data/{R_Patches_Merged,stats}/` to the node's fast local disk.

> **Why the whole repo, not the README's `Data/`+`Models/` mount?** The README's
> single-pipeline recipe bakes code into the image and mounts only data. The
> factorial uses freshly edited Python + shell and writes to `<repo>/results/`;
> mounting the whole tree at `/app` keeps code, stats, and outputs all on the
> persisted `/workdir` copy. `results/` then lives at
> `/workdir/$USER/nys_wetlands/results` — exactly what `rsync_results.sh` pulls.

---

## 6. Launch the factorial (inside `tmux`, via `docker1`)

24 cells ≈ 8–24 GPU-h — it will outlive an SSH session, so run it under
`tmux` (`AGENTS.md` Long-Running Jobs; `screen` works too if you prefer it).

```bash
# On the GPU node:
tmux new -s factorial        # start a persistent session

docker1 run --rm --gpus all --shm-size=8g \
  --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -e TMPDIR=/app/tmp \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh

# Detach (leave it running): press Ctrl-b then d
# Reattach later:            tmux attach -t factorial
```

Flag rationale:
- **`docker1`** — BioHPC wrapper (== privileged docker). Never bare `docker`.
- **`--gpus all`** — exposes the A6000.
- **`--shm-size=8g`** — DataLoader workers use shared memory; the 64 MB default
  causes `bus error` crashes.
- **`--user $(id -u):$(id -g)`** — output files are owned by you, not root.
- **`-v /workdir/$USER/nys_wetlands:/app`** — the only allowed mount location;
  `/app` is the image's `WORKDIR`, so the scripts resolve the repo root to the
  mounted tree.
- **`--rm`** — the container is disposable; all state is on the mounted volume.

**Run a subset** (e.g. just the two label-block cells, or finish one config):

```bash
# only specific configs, all seeds:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -e CONFIGS="nwi_chmret_leafoff flddeg_chmret_leafoff" \
  nys-wetlands-dl bash Shell_Scripts/run_factorial.sh

# one config wrapper directly:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_fld_chmret_leafoff.sh

# dry run (print every command, train nothing):
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -e DRY_RUN=1 nys-wetlands-dl bash Shell_Scripts/run_factorial.sh
```

---

## 7. Monitor, resume, sync back

**Monitor** (from the GPU node, while the container runs):
```bash
ls /workdir/$USER/nys_wetlands/results/*/seed*/metrics.json | wc -l   # cells done (of 24)
tail -f /workdir/$USER/nys_wetlands/results/<config>/seed<k>/train.log
```

**Monitor with TensorBoard** (loss/IoU curves, all cells in one dashboard).
`dl_04_train_lightning.py` writes TensorBoard event files to
`results/<config>/seed<k>/tb_logs/`. TensorBoard is only a *reader* of those
files, so it runs on the **host** (outside the training container) — pointing
`--logdir` at the `results/` root makes every (config × seed) cell appear as its
own run, and new cells show up as the factorial reaches them.
`Shell_Scripts/run_tensorboard.sh` wraps this (default logdir
`/workdir/$USER/nys_wetlands/results`, default port 6006):

```bash
# On the GPU node, in a SECOND screen/tmux window (NOT inside the container):
cd /workdir/$USER/nys_wetlands
Shell_Scripts/run_tensorboard.sh            # USE_SCREEN=1 to self-detach into `screen -S tensorboard`

# Then from your laptop, tunnel the port and open http://localhost:6006
ssh -N -L 6006:cbsugpu10.biohpc.cornell.edu:6006 $USER@cbsulogin.biohpc.cornell.edu
```

The script needs `tensorboard` importable in *some* env on the node — no conda
required (it's a declared project dependency). It auto-uses `tensorboard` if it's
on PATH, else falls back to `uv run`. With **neither uv nor a project env on the
node**, make a throwaway venv just for the reader:

```bash
python -m venv /workdir/$USER/tb_env          # module load python/3.12.7 first if needed
source /workdir/$USER/tb_env/bin/activate
pip install setuptools tensorboard            # setuptools is required: TensorBoard imports
                                              # pkg_resources, which Python 3.12 venvs omit by
                                              # default (ModuleNotFoundError without it)
Shell_Scripts/run_tensorboard.sh
```

**Resume across reservation windows** — just rerun the same `run_factorial.sh`
command. Each cell with a `metrics.json` + `manifest.json` is skipped; only
unfinished cells run. Failed cells are reported in the summary and retried on the
next pass.

**Sync results back to `/ibstorage`** (run from the CPU node, or wherever you
analyze):
```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v3" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results" \
  Shell_Scripts/rsync_results.sh --metrics-only      # fast: JSON/CSV/PNG only
  # drop --metrics-only to also pull the ~500 MB checkpoints
  # add -n / --dry-run first to preview
```

Each cell produces, under `results/<config>/seed<k>/`:
`manifest.json` (fully self-describing: bands, in_channels, label source, class
weights, loss, arch, git commit, stats files, degrade provenance), `metrics.json`,
`confusion_matrix.csv`, the best checkpoint (`.safetensors`/`.ckpt`), and
`train.log` / `eval.log`.

---

## 8. Phase 3 — aggregation & SHAP

Built (`dl_08_aggregate_factorial.py`, `dl_09_shap_factorial.py`). The split
mirrors the node split: **aggregation is CPU/pandas, SHAP is GPU** (it backprops
through each model, so it must run inside the container before reservation
teardown). Forest-restricted metrics (CHM-threshold mask) are **not yet built** —
deferred; `dl_08` will pick up a `metrics_forest.json` automatically if a future
runner writes one.

**Aggregation (CPU node, after sync-back — safe to run on a partial tree):**

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
Shell_Scripts/run_aggregate.sh                  # RESULTS_DIR defaults to Models/factorial_results
# or call the script directly to point elsewhere:
#   python Python_Code_Analysis/DL_Pipeline_v2/dl_08_aggregate_factorial.py --results-dir results
# writes <results-dir>/analysis/:
#   factorial_long.csv      per (config, seed, class) precision/recall/f1/iou
#   factorial_summary.csv   mean & sd over seeds
#   factorial_table.csv     headline pivot: FSW/UPL IoU+recall, macro-F1 (mean±sd)
#   contrasts.csv           paired-by-seed effects (LiDAR tiers, leaf-off, interaction, label gradient)
#   confusion_mean/<config>.csv   seed-mean confusion matrix (the FSW↔UPL cells)
#   coverage.csv            which (config × seed) cells are present
```

Contrasts are **paired by seed** — the same seed gives the same split across all
8 configs (Section 3), so per-seed differences net out split luck before the
mean±sd. The script prints coverage (e.g. `17/24 cells`) and computes every
contrast it has the cells for, so re-running as the factorial fills in just
extends the table.

**SHAP (GPU node, in the container, before teardown):**

```bash
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl \
  python Python_Code_Analysis/DL_Pipeline_v2/dl_09_shap_factorial.py
# default scope: all FIELD configs (label==fld) × every seed dir present.
#   --configs fld_chmret_leafoff    one config
#   --seeds 0                        one seed
#   --n-background / --n-test / --crop-size   memory/cost knobs
# writes results/<config>/seed<k>/shap/ ; idempotent. Per cell:
#   *_shap_band_importance*.png / *_shap_summary_plot.png   figures
#   *_shap_importance.json    per-band importance, BOTH aggregations:
#       importance_overall(_per_class)             = SUM over a band's channels
#       importance_overall(_per_class)_per_channel = MEAN (sum / n_channels)
#       n_channels                                 = channels per band
#   *_shap_per_channel.npz    spatially-averaged per-channel |SHAP|
#       (shap_abs (n_classes, n_test, C_input) + channel_band map) -- the raw
#       array the JSON aggregates; enables the within-Geomorphon form breakdown.
```

`dl_09` is band-correct for free: it points `--stats-path` at each config's
per-config stats, and `WetlandPatchDataset` subsets bands by that file's
`predictor_names`. The SHAP outputs sync back with `rsync_results.sh` (they live
under `results/`). Pair `contrasts.csv` (ablation = marginal contribution)
against the SHAP importance JSON (reliance) for the feature story.

**Why both aggregations.** A band's channels are summed back to band level, but
the one-hot `Geomorph_local` band is 10 channels while every continuous band is 1
— so the SUM inflates it ~10×. The per-channel MEAN (`sum / n_channels`) is the
fair comparison; the truth sits between. `dl_10_factorial_viz.ipynb` §4a shows
both side by side, and §4b uses the `.npz` to split Geomorphon into its 10 forms
(is the signal concentrated in a few wet-landform forms, or smeared = artifact?).

### 8.1 Re-importing to the GPU node and re-running SHAP

When the GPU node was cleaned (reservation ended) but you have updated SHAP code
or want the newer outputs (the `.npz` + per-channel JSON fields above), restage
and re-run. **Two flags are mandatory or the run silently does nothing:**

- **`--force`** — `dl_09` skips any cell that already has a `*_shap_importance.json`
  (idempotency). Every field cell has one from the last pass, so without `--force`
  *all* cells are skipped and no new `.npz` / per-channel fields are written.
- **`--results-dir Models/factorial_results`** — `dl_09` defaults `--results-dir`
  to `<repo>/results`, but the local archive keeps the trained cells in
  `Models/factorial_results/` (that is where the original `results/` synced back
  to). Point it there so it finds the checkpoints with the §5 whole-repo push
  unchanged. (Alternatively restage the cells into the node's `results/`.)

SHAP needs, on the node: the **updated code**, each cell's **`best_*.safetensors`
(+ `.meta.json` sidecar)** — *not* the `.ckpt` (≈36 GB of dead weight; `run_shap`
prefers safetensors and auto-detects arch from the sidecar) — the **per-config
`stats/`**, and the **patches** (the SHAP background/test split). Lean push from
the CPU node (skips the `.ckpt`, ≈13 GB instead of ≈49 GB):

```bash
# FROM the CPU node:
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU_NODE=cbsugpu10.biohpc.cornell.edu        # your reserved node

rsync -avhP --relative \
  --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 \
  Shell_Scripts \
  Data/Training_Data/stats \
  Data/Training_Data/R_Patches_Merged \
  Models/factorial_results \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

`--relative` recreates each path under the dest, so the node mirrors the local
tree (`…/nys_wetlands/Models/factorial_results` → `/app/Models/factorial_results`
once mounted).

Run via the wrapper (`run_shap_factorial.sh` handles the two container quirks:
writable `HOME`/`MPLCONFIGDIR`, and `pip install --user shap` if the image lacks
it — needs node internet or shap pre-baked). 18 field cells, GPU, ~minutes each —
run it under `tmux` (matching §6; `screen` works too) so an SSH drop won't kill it:

```bash
# ON the GPU node:
tmux new -s shap
cd /workdir/$USER/nys_wetlands
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
    --results-dir Models/factorial_results --force
# Detach: Ctrl-b then d   |   reattach: tmux attach -t shap
# add --configs / --seeds to scope; default is all field configs × seeds present.
# include nwi/flddeg only by naming them in --configs.
```

Pull back just the SHAP artifacts (not the unchanged ~11 GB of safetensors):

```bash
# FROM the CPU node:
rsync -avhP \
  --include='*/' \
  --include='*_shap_importance.json' \
  --include='*_shap_per_channel.npz' \
  --include='*_shap_*.png' \
  --exclude='*' \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/Models/factorial_results/" \
  /ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results/
```

Then re-run `dl_10_factorial_viz.ipynb` — no `run_aggregate.sh` needed (SHAP does
not feed `dl_08`); §4a/§4b pick up the new outputs automatically.

---

## 9. Pre-launch checklist

- [ ] `R_Patches_Merged/` current (re-merged + re-degraded if `R_Patches` / `R_Patches_NWI` changed)
- [ ] `dl_experiment_config.py` self-check passes (channel matrix OK)
- [ ] master `multiclass_normalization_stats_wp0.5.json` current (rebuilt via `dl_01 --global-stats` if the R scan / bands / patches changed)
- [ ] `Data/Training_Data/stats/` has all 8 `..._wp0.5.json` files (re-derived from the master)
- [ ] `dl_preflight_check.py` is green (0 fail; warnings are advisory)
- [ ] repo + `Data/` rsynced over ssh to `/workdir/$USER/nys_wetlands`; image loaded (`docker1 images`)
- [ ] `run_factorial.sh` launched inside `tmux`; mount only under `/workdir/$USER`
- [ ] `rsync_results.sh -n` dry-run round-trips before the first real pull
- [ ] heavy jobs run by **the user** from these scripts (agent does not auto-execute)

---

## 10. Follow-on studies (plan Section 9)

Studies on top of the base factorial. Each reuses the idempotent `run_config.sh`
via env knobs (`ARCH`, `N_PATCHES`, `CELL_NAME`, `CAT_CHANNELS`,
`DEEP_SUPERVISION`, `GATE_KERNEL`) that default to base-factorial behavior, so
the base grid is untouched.

> **v3 changes to this section.** The architecture comparison now has **three**
> arms — U-Net (base grid), UNet3+ (`run_arch_compare.sh`), and the multi-branch
> fusion encoder (`run_arch_fusion.sh`, see §10.4). The **patch-count learning
> curve is dropped from v3** (`run_patchcurve.sh` still works and is unchanged —
> it is deferred until the training pool reaches 1000s of patches, since the
> current 100–500 range is under one order of magnitude and won't support a
> scaling claim). Results roots moved to the `_v3` namespace:
> `Models/factorial_results_v3/`, `Models/results_arch_v3/`,
> `Models/results_arch_fusion_v3/`. Pick `<config>` from `factorial_table.csv` (the
best deployable field config, likely `fld_chmret_leafoff`). Heavy runs go on the
GPU node inside `tmux`/`docker1` exactly like `run_factorial.sh`; aggregation is
pure pandas on the CPU node. Validate the plan first with `DRY_RUN=1`.

### 10.0 Setup recap — reload the node first (same ritual as §4–§7)

A follow-on study usually starts **after the base-factorial reservation ended**,
so the GPU node's `/workdir` was wiped: the image is gone, the repo/data are gone,
and the new results roots don't exist yet. The three study blocks below show only
the *study-specific* command — they assume you've done these four steps first
(this is the part that was implicit before). Skip what's already current.

**1. Reload the image** (§4) — gone with the reservation:

```bash
# ON the GPU node:
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz
docker1 images | grep nys-wetlands-dl     # confirm
```

**2. Restage repo + data** from the CPU node — the lean §8.1 push is the right
template (skips the ~36 GB of `.ckpt`; the studies need code, `stats/`, patches,
and the trained cells). `Models/factorial_results` is only needed by the
arch-compare baseline pairing and the predict checkpoint lookup — drop it for a
pure patch-curve run:

```bash
# FROM the CPU node:
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU_NODE=cbsugpu10.biohpc.cornell.edu          # your reserved node
rsync -avhP --relative \
  --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 \
  Shell_Scripts \
  Data/Training_Data/stats \
  Data/Training_Data/R_Patches_Merged \
  Models/factorial_results \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

**3. The canonical container wrapper** — identical to §6; every "in the container"
line in the blocks below drops into the final slot. Launch under `tmux` so it
outlives the SSH session:

```bash
# ON the GPU node:
tmux new -s followon
cd /workdir/$USER/nys_wetlands
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -e TMPDIR=/app/tmp \
  nys-wetlands-dl \
  bash Shell_Scripts/<study-script>.sh <args>
# Detach: Ctrl-b then d   |   reattach: tmux attach -t followon
```

> **Predict study only:** its source rasters live at `/workdir/$USER/NYS_Wetlands_Data`,
> **outside** the `/app` mount, so that wrapper needs a **second `-v`**
> (`-v /workdir/$USER/NYS_Wetlands_Data:/data` + `-e DATA_ROOT=/data`). The predict
> block below shows the full two-mount form — don't use the single-mount wrapper there.

**4. Sync results back** (§7) — each study writes a **new** results root on the GPU
node (`results_patchcurve/`, `results_arch/`). Pull each **under `Models/`** on the
CPU node — same convention as the base factorial's `Models/factorial_results` — via
`rsync_results.sh`'s env vars, then aggregate. The aggregation commands below point
`--results-dir` / `--arch-dir` at those `Models/…` paths (`dl_08b` resolves
relative paths against the repo root):

```bash
# FROM the CPU node, per study (patch-curve shown; arch study is the same shape):
cd /ibstorage/anthony/NYS_Wetlands_DL
SERVER="$USER@$GPU_NODE:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/results_patchcurve" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/results_patchcurve" \
  Shell_Scripts/rsync_results.sh --metrics-only      # -n to preview first
#   arch study:  REMOTE_RESULTS=.../results_arch   LOCAL_DEST=.../Models/results_arch
```

> The **predict** study writes GeoTIFFs to `Data/HUC_DL_Predictions/`, not a
> `results/` tree, so pull those with a plain rsync instead:
> `rsync -avhP "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/Data/HUC_DL_Predictions/" Data/HUC_DL_Predictions/`

---

**Patch-count learning curve** (`results_patchcurve/<config>_n<level>/seed<k>/`)
— **not part of v3.** Dropped in favour of the third architecture arm; the
script and this block stay so the v2 curve reproduces.

```bash
# GPU node — drop into the §10.0 wrapper's final slot as <study-script> <args>
#   (18 cells: 6 levels × 3 seeds; idempotent/resumable):
bash Shell_Scripts/run_patchcurve.sh fld_chmret_leafoff
#   LEVELS="100 200 300 400 500 full"  SEEDS="0 1 2"   (override via env)
# CPU node, after sync-back (synced to Models/results_patchcurve per §10.0 step 4):
python Python_Code_Analysis/DL_Pipeline_v2/dl_08b_aggregate_patchcurve.py \
    --results-dir Models/results_patchcurve
#   -> Models/results_patchcurve/analysis/{patchcurve_long.csv,patchcurve_summary.csv,patchcurve.png}
#   x-axis = REALIZED #train patches (training_log.json data_split), not the cap.
```

**Architecture comparison** — v3 runs **three arms** on one config: the U-Net base
grid, UNet3+ (`results_arch_v3/<config>_unet3plus/seed<k>/`), and the fusion
encoder (§10.4).

```bash
# GPU node — in the §10.0 wrapper's final slot (5 cells per mode; deep-supervision
#   ON; bf64/d5 held = fair vs U-Net baseline):
bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff
MODE=binary bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff
#   BATCH_SIZE defaults to 8; drop to 4 on OOM. eval auto-detects arch.
# The U-Net arm already exists in the base factorial — on the CPU node that is the
# synced Models/factorial_results_v3/<mode>/<config>/ (same seeds; all three
# drivers default to SEEDS="0 1 2 3 4").
# CPU node — one --arch-dir per arm, run once per mode:
python Python_Code_Analysis/DL_Pipeline_v2/dl_08b_aggregate_patchcurve.py \
    --arch-compare --config fld_chmret_leafoff --mode multiclass \
    --arch-dir unet=Models/factorial_results_v3 \
    --arch-dir unet3plus=Models/results_arch_v3 \
    --arch-dir mbfusion=Models/results_arch_fusion_v3
```

Four CSVs land in the **last** arm's `<root>/<mode>/analysis/` (override with
`--output-dir`):

| File | What it holds |
|---|---|
| `arch_compare_long.csv` | one row per (arch, seed) — every metric plus cost. The tidy form; plot from this. |
| `arch_contrasts.csv` | paired per-seed deltas vs the baseline arm (first `--arch-dir`, or `--baseline`), with `n_better`/`n_seeds`. |
| `arch_cost.csv` | params, GFLOPs, sec/epoch per arm, and params as a multiple of the baseline's. |
| `arch_compare.csv` | wide per-seed table + seed-mean row (the v2-shaped output). |

Two things to know about the contrasts. **`--confusion-pair` (default `FSW UPL`)**
adds row-normalized directional confusion rates — the share of true-FSW pixels
predicted UPL and vice versa — which is the specific failure the fusion encoder
targets; in binary mode neither class exists, so those rows are simply absent.
And **at n=5 the credible summary is sign consistency**, `n_better`/`n_seeds`, not
a p-value: same seed ⇒ same test patches ⇒ each delta is genuinely paired, but
five paired differences do not support a distributional claim. No p-values are
computed, deliberately.

`--unet-dir` / `--unet3plus-dir` still work as deprecated two-arm aliases, so the
v2 arch-compare output reproduces from the same script.

**Prediction / inference maps** (per HUC: class + per-class softmax probs). Unlike
training, this needs the **source rasters** for the target HUC on the prediction
node — pull only the 7 per-HUC tiles (~4–5 GB/HUC), not the ~1.74 TB tree:

```bash
# Prereq: §10.0 steps 1–2 (image loaded, repo+Models/factorial_results staged).
# This study uses a TWO-mount wrapper (source rasters live outside /app) — not the
# single-mount §10.0 wrapper.

# 1. Sync source rasters for the HUC (FROM wherever NYS_Wetlands_Data lives):
SERVER="$USER@$DATA_HOST:" REMOTE_ROOT=/ibstorage/anthony/NYS_Wetlands_Data \
LOCAL_ROOT=/workdir/$USER/NYS_Wetlands_Data \
  bash Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>     # -n to preview

# 2. Verify the band/channel contract — in the container, mounting the data tree at /data:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -v /workdir/$USER/NYS_Wetlands_Data:/data \
  nys-wetlands-dl \
  python Python_Code_Analysis/DL_Pipeline_v2/dl_huc_stack.py \
    --huc <huc> --cluster <cluster> --data-root /data --inspect

# 3. Predict with the best checkpoint (best-macro-F1 seed by default) — same two mounts:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -v /workdir/$USER/NYS_Wetlands_Data:/data \
  -e DATA_ROOT=/data \
  nys-wetlands-dl \
  bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>
#   -> Data/HUC_DL_Predictions/DLpred_<mode>_cluster_<C>_huc_<H>.tif  (class)
#                              ..._probs.tif                          (per-class softmax)
#   RESULTS_DIR defaults to Models/factorial_results (then results/). Append a seed
#   arg to pin one; works unchanged for a UNet3+ checkpoint (arch auto-detected).
```

Start with 2–3 demo HUCs to validate before scaling up.

---

### 10.4 Multi-branch fusion encoder (`mbfusion`) — the third architecture arm

Design and rationale: `../arch_fusion/PLAN.md`. A per-input-category encoder
(terrain / lidar / leafon / leafoff) fused at every scale by a per-pixel,
softmax-normalized gate, with a decoder **bit-identical** to the U-Net's — so the
comparison isolates encoder + fusion as the only changed variable.

```bash
# GPU node, in the container, inside tmux:
bash Shell_Scripts/run_arch_fusion.sh fld_chmret_leafoff              # multiclass
MODE=binary bash Shell_Scripts/run_arch_fusion.sh fld_chmret_leafoff  # binary
```

Writes `Models/results_arch_fusion_v3/<mode>/<config>_mbfusion/seed<k>/`.

**The branch map is not a knob.** The trainer derives it from the config's stats
file (`stats["predictor_names"]`, in post-one-hot-expansion channel space) and
stores it in the checkpoint + `.meta.json`, so eval/predict auto-detect it and a
`nolidar`/`leafon` config simply yields fewer branches. `dl_preflight_check` **[9]**
gates the map on CPU before any GPU time — this is the one silent failure mode
(a wrong map trains fine and reports plausible numbers while each encoder reads
the wrong bands).

**Seeds — all three arms must match.** The paired per-seed comparison uses the
*intersection*, so a short arm silently shrinks n rather than erroring. All three
drivers default to `SEEDS="0 1 2 3 4"`, so a completed v3 grid already supplies the
U-Net arm at the right seeds — no top-up run. `run_arch_fusion.sh` prints a
seed-coverage table per arm when it finishes; check it before aggregating.

**Memory.** Params are ~1.3× the U-Net (162M vs 125M at bf64/d5/29ch), but the
binding constraint is *activations*: at level 0 the fused tensor is 144 channels
at 256² against the U-Net's 64 — 2.25× the finest-scale activation. Defaults to
`BATCH_SIZE=8`; halve to 4 on CUDA OOM. Lighter than UNet3+.

**Watch for gate collapse.** TensorBoard scalars `train/gate_entropy/level0..5`.
Healthy is near `log(n_branch)` (1.386 for 4 branches); trending toward 0 in the
first few epochs means the gate has collapsed onto one branch. The standard fix is
a temperature on the gate logits — deliberately not built in speculatively.

**Gate rasters (a deliverable, not a debug artifact).** Export before teardown:

```bash
python $PIPE/dl_11_export_gates.py \
  --cell Models/results_arch_fusion_v3/multiclass/fld_chmret_leafoff_mbfusion/seed0 \
  --config fld_chmret_leafoff --seed 0 --mode multiclass
```

Writes `<cell>/gates/<patch>.npz` (six float16 `(n_branch, H, W)` arrays, ~0.5 MB
per patch) + `gate_summary.json`, from a deterministic prefix of the seed's
held-out field patches. `rsync_results.sh --metrics-only` now includes `*.npz`, so
they come back with the JSON/CSV — and `*.log`, because Lightning's model summary
in `train.log` is the only source of the GFLOPs column in `arch_cost.csv`.

**Aggregating the fusion arm** is just the three-arm `--arch-dir` command above;
nothing is fusion-specific about it. Cost columns come from the trainer's journal
(`cost` block: exact params, fit-only wall clock, epochs actually run), added in
v3 — v1/v2 cells fall back to the `.safetensors` header for params and report
timing as blank rather than reconstructing a guess from file mtimes.

Figures live in `R_Code_Analysis/dl_10_Factorial_viz_R.qmd`: set `arch_dir_base`
to the fusion root and the architecture section renders all three arms, the
FSW↔UPL confusion panel, the contrast and cost tables, and the gate-weight plot.
It reads `arch_compare_long.csv`, so a fourth arm needs only a colour.

> **Reading gate maps — the one caveat.** After gating, `proj` is a 1×1 conv, so
> the decoder sees `Σᵢ Wᵢ(fᵢ·gᵢ)`. **Valid:** within-branch spatial comparison
> ("terrain reliance rises in depressions relative to sideslopes") — the gate is
> the only thing varying across space. **Confounded:** cross-branch absolute
> comparison ("terrain matters more than LiDAR overall"), since a branch with
> modest gates but large `Wᵢ` can still dominate. GroupNorm equalizes features,
> not projection weights. Plot gates **standardized within branch**, and take
> overall branch importance from **SHAP**. The means in `gate_summary.json` are
> provenance, not a ranking.

## 11. Repeating the experiment (a versioned v2 run)

When the inputs change — **more training patches**, or **new predictor bands** —
you re-run the whole grid as a *new version* (`v2`, `v3`, …). The hazard is that
almost every output writes to a **fixed path** and would silently overwrite or
mask v1. This section is the ritual that keeps v1 intact and reproducible while
v2 runs alongside it.

> **One convention drives the whole thing:** export `EXP_VERSION` and derive
> every output root from it. Most roots are already env knobs, so this needs
> almost no code change — the two exceptions are called out in §11.5.

### 11.1 First: why a naïve repeat is unsafe

Three classes of artifact overwrite **in place**, and `Models/`+`Data/` are
gitignored, so a git tag preserves *code and docs* but **not** results, stats, or
patches. The footguns:

| Artifact | Fixed path | What a repeat does |
|---|---|---|
| Trained cells | `Models/factorial_results/<config>/seed<k>/` | Idempotent skip sees v1's `metrics.json`+`manifest.json` and **skips every cell** — v1 results silently masquerade as a fresh run. Delete to force, and v1 is destroyed. |
| Per-config stats | `Data/Training_Data/stats/*_<config>_wp0.5.json` | `dl_make_config_stats --all` overwrites them; the master `multiclass_normalization_stats_wp0.5.json` too. v1 metrics survive, but v1 **SHAP / predict** (which reload stats) can no longer be reproduced. |
| Merged patches | `Data/Training_Data/R_Patches_Merged/<name>.tif` | Re-merge rewrites each file. If predictors changed, the band schema changes and v1 checkpoints no longer load against these patches. |
| HUC predictions | `Data/HUC_DL_Predictions/DLpred_<mode>_cluster_<C>_huc_<H>.tif` | Filename has no version token → a v2 prediction of the same HUC overwrites v1. |
| Viz notebook | `dl_10_factorial_viz.ipynb` (hardcoded `Models/factorial_results` ×9, `results_arch`, `results_patchcurve`) | Re-running repoints at whatever is on disk and overwrites the rendered v1 figures. |

### 11.2 Snapshot v1, then fork the namespace

**Step A — freeze v1 code + docs (git):**

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
git tag factorial-v1          # freezes dl_experiment_config.py, this EXECUTION.md, the plan, the notebook
```

**Step B — snapshot v1's gitignored artifacts (physical copies).** These are the
only way to keep v1 SHAP/predict reproducible after the inputs change. Copy, do
not move — v2 prep regenerates the canonical paths:

```bash
cp -a Models/factorial_results            Models/factorial_results_v1
cp -a Data/Training_Data/stats            Data/Training_Data/stats_v1
cp -a Data/Training_Data/multiclass_normalization_stats_wp0.5.json \
      Data/Training_Data/multiclass_normalization_stats_wp0.5_v1.json
cp -a Data/Training_Data/R_Patches_Merged Data/Training_Data/R_Patches_Merged_v1   # only if predictors/patches change the schema
# follow-on roots, if present:
cp -a Models/results_patchcurve Models/results_patchcurve_v1 2>/dev/null || true
cp -a Models/results_arch       Models/results_arch_v1       2>/dev/null || true
```

### 11.3 The one variable and the knobs it drives

Pick a version and derive every output root from it. These are the **existing**
env knobs (no code change) — set them once per shell:

```bash
export EXP_VERSION=v2
export RESULTS_DIR="$PWD/Models/factorial_results_${EXP_VERSION}"   # run_config/run_factorial
export PATCHES_DIR="$PWD/Data/Training_Data/R_Patches_Merged"       # v2 schema (regenerated in §11.4)
export OUT_DIR="$PWD/Data/HUC_DL_Predictions_${EXP_VERSION}"        # run_predict_factorial
```

| Output | Knob | Status |
|---|---|---|
| Trained cells | `RESULTS_DIR` | ✅ env knob today |
| Merged patches | `PATCHES_DIR` | ✅ env knob today |
| Master stats | `dl_01 --weight-power` (path derived) or `--output` / `dl_make_config_stats --master-stats` | ✅ flags |
| Predictions | `OUT_DIR` (predict) | ✅ env knob today |
| Aggregation | `dl_08*.py --results-dir` | ✅ flag |
| **Per-config stats dir** | `STATS_DIR` in `run_config.sh` | ⚠️ **hardcoded** to `Data/Training_Data/stats` (line 42) — see §11.5 |
| **Viz notebook root** | top-of-notebook literal | ⚠️ **hardcoded** ×~17 — see §11.5 |

Because the stats *dir* is fixed, the safe pattern today is **snapshot v1 stats
(§11.2 step B), then regenerate `Data/Training_Data/stats/` for v2 in place** —
the runner reads the canonical dir, so v2's stats simply *are* the canonical
stats once v1 is copied aside. (§11.5 offers the one-line knob that removes even
this.)

### 11.4 Run v2

The flavor of change decides how much you touch:

**Flavor 1 — more patches, same bands (no schema change).** Lowest risk; **no
code edits**. Re-run prep so the patch count re-feeds the field class weights
(EXECUTION §3 step 0 → 4), writing the master to a v2 name:

```bash
PIPE=Python_Code_Analysis/DL_Pipeline_v2
python $PIPE/dl_merge_nwi_labels.py && python $PIPE/dl_degrade_labels.py --seed 0   # rebuild R_Patches_Merged
python $PIPE/dl_experiment_config.py                                                 # channel matrix self-check
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir  Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power 0.5                          # -> ..._wp0.5.json master (prior version copied aside first)
python $PIPE/dl_make_config_stats.py --all                                           # regenerates Data/Training_Data/stats/
python $PIPE/dl_preflight_check.py                                                   # must be GREEN
```

**Flavor 2 — new predictor band(s) (schema change).** Invasive; edit code
*before* prep, in one commit so the preflight stays the guardrail:

1. `dl_experiment_config.py` — add the band to `BASE_BANDS` / `LIDAR_TIERS` /
   `SPECTRAL_TIERS`, **and** update the 8 `"channels"` values, or
   `verify_channel_matrix()` (and thus the preflight) fails.
2. `dl_band_config.json` — add the new band's normalization rule.
3. Rebuild the global raster scan (`HUC_DL_Stacks_Extracted_Values.json`) in the
   sibling `NYS_Wetlands_Data` project so the new band has global min/max.
4. Update the channel-count tables in **this file (§2a)** and the plan.
5. Then run the Flavor-1 prep block above.

> **in_channels changing means v1 checkpoints are incompatible** — no warm-start;
> v2 is a full retrain. That is expected and is exactly why v1 was snapshotted.

**Train + aggregate (both flavors), with the knobs from §11.3 exported:**

```bash
# GPU node, in the container (§6 wrapper), with RESULTS_DIR/PATCHES_DIR passed through -e:
bash Shell_Scripts/run_factorial.sh                 # writes Models/factorial_results_v2/ (fresh dir ⇒ every cell runs)
# CPU node, after sync-back:
Shell_Scripts/run_aggregate.sh                      # RESULTS_DIR already points at the v2 dir
# Predictions land in Data/HUC_DL_Predictions_v2/ via OUT_DIR.
```

The idempotent skip is now a **feature**: a fresh `RESULTS_DIR` has no
`metrics.json`, so all 24 cells train; stop/resume still works within v2.

### 11.5 The two code gaps (prepared, optional)

Everything above works today. Two one-line edits would make `EXP_VERSION` fully
knob-driven (no snapshot-then-overwrite for stats, no notebook hand-edit):

- **`run_config.sh` line 42** — make the stats dir an env knob:
  `STATS_DIR="${STATS_DIR:-$DATA/stats}"`. Then `export STATS_DIR=…/stats_v2` and
  point `dl_make_config_stats --out-dir` at the same path; v1's `stats/` is never
  touched.
- **`dl_10_factorial_viz.ipynb`** — hoist the ~17 hardcoded result roots to one
  `RESULTS_ROOT = Path("Models/factorial_results")` cell at the top, so v1 and v2
  figures render from separate trees without clobbering each other.

### 11.6 v2 pre-launch checklist

- [ ] `git tag factorial-v1` created (code/docs frozen)
- [ ] v1 artifacts copied aside (§11.2 step B): `factorial_results_v1`, `stats_v1`, master `_v1.json`, and `R_Patches_Merged_v1` if the schema changed
- [ ] `EXP_VERSION` exported; `RESULTS_DIR` / `PATCHES_DIR` / `OUT_DIR` derived from it
- [ ] Flavor 2 only: `dl_experiment_config.py` channels + `dl_band_config.json` + global scan + §2a tables updated **in one commit**
- [ ] prep re-run (merge → degrade → master stats → `dl_make_config_stats --all` → preflight GREEN)
- [ ] `DRY_RUN=1 RESULTS_DIR=… bash Shell_Scripts/run_factorial.sh` confirms cells target the v2 dir before any GPU time
