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
python Python_Code_Analysis/DL_Pipeline_v2/dl_preflight_check.py --require-all-labels  # must be green

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
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/results" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results" \
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
| `<label>` | `fld` / `nwi` / `flddeg` | training **label source**: field-verified / NWI / field degraded to NWI prevalence |
| `<lidar>` | `nolidar` / `chm` / `chmret` | LiDAR tier: none / CHM only / CHM + return-fraction bands |
| `<spectral>` | `leafon` / `leafoff` | leaf-on NAIP only / + leaf-off NAIP RGB+NIR |

So `fld_chmret_leafoff` = field labels, full LiDAR, both seasons (the full
26-channel feature set); `nwi_chmret_leafoff` = same features, NWI labels.

The **8 configs** (feature ablation runs on field labels only; the label
comparison runs on the full feature set only — see plan §2):

| Config | `in_channels` |
|---|---|
| `fld_nolidar_leafon`    | 18 |
| `fld_nolidar_leafoff`   | 22 |
| `fld_chm_leafon`        | 19 |
| `fld_chm_leafoff`       | 23 |
| `fld_chmret_leafon`     | 22 |
| `fld_chmret_leafoff`    | 26 |
| `nwi_chmret_leafoff`    | 26 |
| `flddeg_chmret_leafoff` | 26 |

× 3 seeds (0,1,2) = **24 cells**.

### 2b. Python mechanism scripts (`Python_Code_Analysis/DL_Pipeline_v2/`)

| File | Role in the experiment |
|---|---|
| `dl_experiment_config.py` | **Single source of truth.** Band matrix + the 8 configs. `--list` prints config names; `--emit <config>` prints shell vars (`TRAIN_STATS`, `EVAL_STATS`, `IN_CHANNELS`, …) consumed by `run_config.sh`; no args runs the channel-matrix self-check. |
| `dl_merge_nwi_labels.py` | Phase 0: writes `MOD_CLASS_NWI` band into each patch (already done → `merge_manifest.json`). |
| `dl_degrade_labels.py` | Phase 1.3: writes `MOD_CLASS_FLDDEG` (field wetland→UPL down to NWI prevalence, seeded). Already done → `degrade_manifest.json`. |
| `dl_make_config_stats.py` | Phase 1.4: subsets the master stats into the 8 per-config stats files. `--all` writes all of them. |
| `dl_preflight_check.py` | Phase 0 gate: same patch set, identical footprints, predictor parity, label-value sanity, channel sanity. `--require-all-labels` insists all three label bands exist. **Must be green before any GPU time.** |
| `dl_04_train_lightning.py` | Training (called by the runner). |
| `dl_05_evaluate.py` | Test-set metrics + confusion matrix (called by the runner; auto-detects arch from the checkpoint). |

### 2c. Shell orchestration (`Shell_Scripts/`)

| Script | What it does |
|---|---|
| `run_config.sh <config> <seed>` | **The workhorse.** Runs *one* cell: resolves stats via `dl_experiment_config.py --emit`, trains, finds the best checkpoint, evaluates on the **field** test set, writes `results/<config>/seed<k>/` (metrics, confusion matrix, manifest, logs). **Idempotent** — skips a cell whose `metrics.json` + `manifest.json` already exist. |
| `run_<config>.sh` (8 of them) | Thin wrapper: loops one config over `SEEDS` (default `0 1 2`), deferring to `run_config.sh`. Use to run a single config's replicates. |
| `run_factorial.sh` | **Top-level driver.** Walks every (config × seed) cell, seed-outer, calling `run_config.sh`; skip-completed makes it resumable. This is what you launch in `tmux`. |
| `rsync_results.sh` | Pull `results/` from the GPU node's `/workdir` back to the CPU node (`--metrics-only` for fast JSON/CSV/PNG, no flag for full checkpoints). |

**Key behavior to remember:** a `nwi`/`flddeg` config *trains* on its own label
source but is *evaluated* against **field** labels (plan §3, non-negotiable). The
runner enforces this automatically by evaluating with the matching
`fld_<lidar>_<spectral>` stats — same seed ⇒ same test patches.

### 2d. Useful env knobs (override on the command line)

`run_config.sh` / `run_factorial.sh` read these from the environment:

| Var | Default | Use |
|---|---|---|
| `SEEDS` | `0 1 2` | replicate seeds (`SEEDS="0 1 2 3 4"` to extend to R=5) |
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

# 1. Confirm the config matrix is self-consistent (channel counts match the plan)
python $PIPE/dl_experiment_config.py            # prints the 8 configs; ends "All channel counts match"

# 2. Generate the 8 per-config stats files (idempotent; skip if stats/ already full)
python $PIPE/dl_make_config_stats.py --all      # -> Data/Training_Data/stats/*.json

# 3. Preflight — HARD GATE before any GPU time
python $PIPE/dl_preflight_check.py --require-all-labels   # expect 0 failures / 0 warnings
```

> As of the last session all three are already satisfied: 526 merged 20-band
> patches, 8 stats files present, preflight green. Re-run them only if the data
> or band matrix changed.

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

**Resume across reservation windows** — just rerun the same `run_factorial.sh`
command. Each cell with a `metrics.json` + `manifest.json` is skipped; only
unfinished cells run. Failed cells are reported in the summary and retried on the
next pass.

**Sync results back to `/ibstorage`** (run from the CPU node, or wherever you
analyze):
```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/results" \
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

## 8. Phase 3 — aggregation & SHAP (pending)

Not yet built — awaiting the user's added steps (plan §3/§5 Phase 3). Drafted
scope: walk `results/<config>/seed*/metrics.json` into the factorial table
(pure pandas, CPU node, post-sync); compute the LiDAR-tier, leaf-off, and
label-gradient contrasts; run SHAP **on the GPU node** (it backprops) before
tearing down the reservation, then sync its outputs back with the rest.

---

## 9. Pre-launch checklist

- [ ] `dl_experiment_config.py` self-check passes (channel matrix OK)
- [ ] `Data/Training_Data/stats/` has all 8 `..._wp0.5.json` files
- [ ] `dl_preflight_check.py --require-all-labels` is green (0 fail / 0 warn)
- [ ] repo + `Data/` rsynced over ssh to `/workdir/$USER/nys_wetlands`; image loaded (`docker1 images`)
- [ ] `run_factorial.sh` launched inside `tmux`; mount only under `/workdir/$USER`
- [ ] `rsync_results.sh -n` dry-run round-trips before the first real pull
- [ ] heavy jobs run by **the user** from these scripts (agent does not auto-execute)
