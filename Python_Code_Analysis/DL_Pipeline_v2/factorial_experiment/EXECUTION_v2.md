# Wetland Factorial Experiment v2 — Execution Guide

The operational runbook for **version 2**. Design/rationale lives in
`wetland_factorial_experiment_plan_v2.md` (the build brief) — this file is the
"how to run it" digest, mirroring the v1 `EXECUTION.md` that guided the first run.

> **v1 is frozen.** `EXECUTION.md` and the v1 code are tagged `factorial-v1`; its
> commands assume a single merged-patch dir and no mode axis, so they are **wrong
> for v2**. Use this file for v2. To run anything v1, check the tag out in a
> worktree (`git worktree add ../wetlands-v1 factorial-v1`).

> **⏳ Build status.** Several steps below depend on v2 mechanism code that is not
> built yet — each is tagged **⏳ pending (§N)** and cross-referenced to the plan
> phase. Steps with no tag work today. §2 is the live checklist. **No experiment
> is run until the user launches it** — Claude prepares scripts only.

---

## 0. TL;DR (target workflow)

```bash
# --- CPU/login node: one-time prep (per mode) ----------------------------
cd /ibstorage/anthony/NYS_Wetlands_DL
export EXP_VERSION=v2
PIPE=Python_Code_Analysis/DL_Pipeline_v2
python $PIPE/dl_experiment_config.py                      # sanity: 8-config channel matrix ✅ works
python $PIPE/dl_make_config_stats.py --all --mode multiclass   # ⏳ pending (§1.4): per-config stats
python $PIPE/dl_make_config_stats.py --all --mode binary       # ⏳ pending (§1.4)
python $PIPE/dl_preflight_check.py --require-all-labels         # ⏳ pending (§0 rewrite): must be GREEN

# --- stage repo + THREE patch dirs onto the GPU node's /workdir ----------
rsync -av --exclude '.git' --exclude '.venv' \
      /ibstorage/anthony/NYS_Wetlands_DL/ \
      $USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/
#   (R_Patches, R_Patches_NWI, R_Patches_NWIextra all ride along under Data/)

# --- launch: loop modes, inside tmux, via docker1 (ON the GPU node) ------
docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz     # first time only
tmux new -s factorial_v2
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app \
  -e EXP_VERSION=v2 \
  -e RESULTS_DIR=/app/Models/factorial_results_v2 \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh          # ⏳ pending (§2): walks (mode × config × seed)
# Ctrl-b then d to detach; tmux attach -t factorial_v2 to return

# --- pull results back to /ibstorage (mode-tokened tree) -----------------
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v2" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v2" \
  Shell_Scripts/rsync_results.sh --metrics-only
```

The runner stays **idempotent** — stop at a reservation's end, rerun next
session, completed cells are skipped. A fresh `factorial_results_v2/` means every
cell runs (v1's `factorial_results/` is never touched).

---

## 1. What's different from v1 (orientation)

| Aspect | v1 | v2 |
|---|---|---|
| Classification | multiclass only | **multiclass + binary**, a full mode axis (every config run twice) |
| LiDAR axis | 3 tiers (nolidar/chm/chmret) | **2 tiers** (nolidar/chmret); CHM grouped with returns |
| Configs | 8 (incl. `fld_chm_*`) | **8** (`fld_chm_*` dropped; `nwiextra`, `nwifield` added) |
| Label storage | multi-band labels in one merged patch (`R_Patches_Merged`) | **separate directories** per source (`R_Patches`, `R_Patches_NWI`, `R_Patches_NWIextra`) |
| Split | held-out split on the merged dir | **field-anchored split** by location key; test always `R_Patches[test_fld]` |
| Results root | `Models/factorial_results/<config>/` | `Models/factorial_results_v2/<mode>/<config>/` |
| Stats | `multiclass_*` only | `multiclass_*` **and** `binary_*` masters + per-config |

Everything else — the two-node CPU/GPU split, `docker1`, `/workdir`-only mounts,
`tmux` for long jobs, weighted-CE loss, `wp0.5`, U-Net bf64/d5 — is **unchanged**;
see `EXECUTION.md` §1/§4/§8 for those mechanics (they still apply verbatim).

---

## 2. Build status — the live checklist

What exists vs. what must be built before a v2 run (plan §7/§10). Claude builds
these on the user's go; the user runs them.

| Piece | State | Plan ref |
|---|---|---|
| `dl_experiment_config.py` — v2 matrix, dir registry, `location_key`, `--emit --mode` | ✅ done | §10.1 |
| `R_Patches`, `R_Patches_NWI` (554 each, paired, `MOD_CLASS`) | ✅ on disk | 4.1 |
| `R_Patches_NWIextra` (~2×, extra same-HUC12 locations) | ⏳ user building | 4.1/8.1 |
| `dl_patch_pools.py` — field-anchored split + leakage guard | ⏳ to build | 1.2 / 4.5–4.6 |
| `dl_02_dataset.py` — accept explicit file lists + `mode` | ⏳ to build | 1.2 |
| `dl_degrade_labels.py` — seeded train/val-only degrade | ⏳ to build | 1.3 |
| `dl_make_config_stats.py --mode` + `binary` master | ⏳ to build | 1.4 |
| `dl_preflight_check.py` — rewrite for separate dirs + leakage gate | ⏳ to build | §0 |
| `run_config.sh` — `MODE` + `STATS_DIR` knobs, mode-tokened `RESULTS_DIR`, resolve `PATCH_DIRS`/`POOL_RULE` | ⏳ to build | 2.1 |
| `run_factorial.sh` — outer `MODE` loop | ⏳ to build | 2.2 |
| wrappers: drop `run_fld_chm_*.sh`; add `run_nwiextra_*.sh`, `run_nwifield_*.sh` | ⏳ to build | 10.4 |
| §11.5 gaps promoted to required: `STATS_DIR` knob, viz-notebook root hoist | ⏳ to build | §11 |

---

## 3. Config & naming (v2)

**8 configs**, each run in **2 modes** × **R seeds** (start R=3 → 48 runs):

```
fld_nolidar_leafon  fld_nolidar_leafoff  fld_chmret_leafon  fld_chmret_leafoff
nwi_chmret_leafoff  nwiextra_chmret_leafoff  nwifield_chmret_leafoff  flddeg_chmret_leafoff
```

Registry is the single source of truth — query it, don't hardcode:

```bash
python $PIPE/dl_experiment_config.py --list                       # config names
python $PIPE/dl_experiment_config.py                              # channel matrix self-check
python $PIPE/dl_experiment_config.py --emit nwifield_chmret_leafoff --mode binary
#   -> CONFIG/MODE/LABEL_SOURCE/IN_CHANNELS/PATCH_DIRS/POOL_RULE/FIELD_TEST_DIR/TRAIN_STATS/EVAL_STATS
```

`PATCH_DIRS` + `POOL_RULE` tell the runner where train/val come from; **test is
always `FIELD_TEST_DIR` (`R_Patches`) at the seed's `test_fld` keys** — every
config/mode is scored on the same field pixels (plan 4.5). The config-name ⇄
wrapper ⇄ `results/<mode>/<config>/` mapping is the whole naming scheme.

---

## 4. One-time prep (CPU node)

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
export EXP_VERSION=v2
PIPE=Python_Code_Analysis/DL_Pipeline_v2

# 1. Channel matrix self-check (source of truth)                     ✅ works today
python $PIPE/dl_experiment_config.py

# 2. Build the TWO masters (only when a master is stale) — per mode   ⏳ §1.4
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir  Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power 0.5 \
  --output       Data/Training_Data/multiclass_normalization_stats_wp0.5.json
#   ...and a binary master (predictors identical; class_names/weights differ).

# 3. Derive per-config stats from the masters, per mode              ⏳ §1.4
python $PIPE/dl_make_config_stats.py --all --mode multiclass
python $PIPE/dl_make_config_stats.py --all --mode binary

# 4. Preflight — HARD GATE before any GPU time                       ⏳ §0 rewrite
python $PIPE/dl_preflight_check.py --require-all-labels
#   v2 asserts: location-key parity (R_Patches ↔ R_Patches_NWI; NWIextra ⊇ NWI),
#   footprint match per shared key, predictor parity, label values {0,1,2,3,255}
#   (binary remap → {0,1,255}), NWI 255-mask == field mask, and the HEADLINE gate:
#   no test_fld key in any config's train/val pool. Must be GREEN for all 8 × 2.
```

> Rebuild the masters only when the R scan / bands / patches changed. Adding
> `R_Patches_NWIextra` changes the NWI/nwiextra/nwifield **class weights** (more
> wetland pixels), so regenerate their per-config stats when it lands.

---

## 5. Stage repo + data onto `/workdir` (GPU node)

From the **CPU node**, push the whole tree — the runner reads edited shell/Python
and the per-config `stats/`, and v2 needs **all three** patch directories:

```bash
GPU_NODE=cbsugpu10.biohpc.cornell.edu
ssh $USER@$GPU_NODE 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -av --exclude '.git' --exclude '.venv' \
  /ibstorage/anthony/NYS_Wetlands_DL/ \
  $USER@$GPU_NODE:/workdir/$USER/nys_wetlands/
```

*Lean push* (skip the ~GBs of `.ckpt`; code + stats + the 3 patch dirs only):

```bash
rsync -avhP --relative --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts \
  Data/Training_Data/stats \
  Data/Training_Data/R_Patches Data/Training_Data/R_Patches_NWI Data/Training_Data/R_Patches_NWIextra \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

Build/load the image exactly as v1 (`EXECUTION.md` §4): `docker1 load -i …`.

---

## 6. Launch (inside `tmux`, via `docker1`)   ⏳ §2

```bash
tmux new -s factorial_v2
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e EXP_VERSION=v2 \
  -e RESULTS_DIR=/app/Models/factorial_results_v2 \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh          # loops MODES="multiclass binary" × configs × seeds
```

- Finish one mode's grid before the other (partial-reservation safety): the
  driver's outer loop is `MODES`, so a stopped run still yields a complete
  multiclass factorial. Override scope with env: `MODES="multiclass"`,
  `CONFIGS="fld_chmret_leafoff nwi_chmret_leafoff"`, `SEEDS="0 1 2"`.
- Dry run first: `DRY_RUN=1 RESULTS_DIR=… bash Shell_Scripts/run_factorial.sh`
  prints every command and confirms cells target `factorial_results_v2/<mode>/…`.
- Flag rationale (`--gpus all` / `--shm-size=8g` / `--user` / `-v …:/app` / `--rm`)
  is unchanged from v1 — see `EXECUTION.md` §6.

Each cell writes `Models/factorial_results_v2/<mode>/<config>/seed<k>/`:
`manifest.json` (now records mode, patch dirs, pool rule, leakage regime),
`metrics.json`, `confusion_matrix.csv`, best `.safetensors`/`.ckpt`, logs.

---

## 7. Monitor, resume, sync back

- Resume: rerun the same `run_factorial.sh` — completed cells (with `metrics.json`
  + `manifest.json`) are skipped.
- TensorBoard: same as v1 (`EXECUTION.md` §7), point it at
  `Models/factorial_results_v2`.
- Sync back to the CPU node (mode-tokened tree):

```bash
SERVER="$USER@cbsugpu09.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/factorial_results_v2" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results_v2" \
  Shell_Scripts/rsync_results.sh --metrics-only    # -n to preview; drop flag for weights
```

---

## 8. Aggregation & SHAP

Split mirrors the node split (aggregation CPU, SHAP GPU). Scope per mode; a
cross-mode comparison table is a v2 addition (plan Phase 3 — **awaiting the
user's analysis spec before building**).

```bash
# Aggregation — CPU node, after sync-back:
RESULTS_DIR=Models/factorial_results_v2/multiclass Shell_Scripts/run_aggregate.sh   # ⏳ mode-tokened
RESULTS_DIR=Models/factorial_results_v2/binary     Shell_Scripts/run_aggregate.sh
# SHAP — GPU node, in-container, before teardown (backprops through each model):
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
  --results-dir Models/factorial_results_v2/multiclass --force
```

Then `dl_10_factorial_viz.ipynb` renders figures — its result roots must be
hoisted to one top cell and branch on `<mode>` (§11.5 gap, now required) before
it can point at the v2 tree without clobbering v1 figures.

---

## 9. Versioning ritual (Flavor 3)

v2 follows the plan's **§11 "Flavor 3"** recipe (new data layout + new configs +
mode axis), which supersedes v1 `EXECUTION.md` §11.4's merge-based prep:

1. `git tag factorial-v1` + snapshot v1 artifacts — **done** (plan §11 Steps A/B).
2. `export EXP_VERSION=v2`; derive `RESULTS_DIR=Models/factorial_results_v2`,
   `OUT_DIR=Data/HUC_DL_Predictions_v2`.
3. Land the §10 code (this file's ⏳ items) in one commit so the preflight stays
   the guardrail.
4. Prep (§4) → preflight GREEN for 8 × 2 → `DRY_RUN=1` confirm → user runs (§6).

See the plan §11 for the full assessment of which v1 §11 steps carry over.

---

## Pointers
- **Design / rationale / decisions:** `wetland_factorial_experiment_plan_v2.md`
- **v1 mechanics that still apply verbatim:** `EXECUTION.md` §1 (node split), §4
  (Docker build/load), §6 (docker flags), §7 (TensorBoard) — read via the tag if
  the working copy has drifted.
- **Follow-on studies (patch curve / UNet3+ / HUC inference):** plan §9 (roots
  gain a `_v2` + `<mode>` token).
