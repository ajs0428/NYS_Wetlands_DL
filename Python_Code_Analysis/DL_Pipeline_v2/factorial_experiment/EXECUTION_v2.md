# Wetland Factorial Experiment v2 — Execution Guide

The operational runbook for **version 2**. Design/rationale lives in
`wetland_factorial_experiment_plan_v2.md` (the build brief) — this file is the
"how to run it" digest, mirroring the v1 `EXECUTION.md` that guided the first run.

> **v1 is frozen.** `EXECUTION.md` and the v1 code are tagged `factorial-v1`; its
> commands assume a single merged-patch dir and no mode axis, so they are **wrong
> for v2**. Use this file for v2. To run anything v1, check the tag out in a
> worktree (`git worktree add ../wetlands-v1 factorial-v1`).

> **📍 Status (2026-07-09) — runs done, analysis pending.** Every GPU stage is
> complete or in flight: base grid **80/80** cells (R=5, both modes), SHAP
> **80/80**, patch curve **60/60**, UNet3+ **10/10**; HUC inference maps running
> (multiclass ✅, binary in progress). The mode-aware analysis layer (`dl_08` +
> `dl_10` §9 cross-mode) is now built and validated; what remains is CPU-side:
> the final sync-back (§11), then running aggregation/viz on a full env (§12).
> **No experiment is run until the user launches it** — Claude prepares scripts only.

---

## 0. The v2 experimental flow (one map, end to end)

Each stage has a section with the exact commands; this table is the spine of
the experiment and its live status (2026-07-09).

| # | Stage | Node | § | Status |
|---|---|---|---|---|
| 1 | One-time prep: config self-check → stats masters → per-config stats → **preflight GREEN** | CPU | §4 | ✅ |
| 2 | Stage repo + 3 patch dirs to `/workdir`; load Docker image | CPU→GPU | §5 | ✅ |
| 3 | Base factorial: 8 configs × 2 modes × 5 seeds = **80 cells** | GPU | §6–§7 | ✅ 80/80 |
| 4 | SHAP per cell (in-container, before teardown) | GPU | §8 | ✅ 80/80 |
| 5 | Follow-on: patch-count learning curve (6 levels × 2 modes × 5 seeds) | GPU | §10 | ✅ 60/60 |
| 6 | Follow-on: UNet3+ architecture comparison (2 modes × 5 seeds) | GPU | §10 | ✅ 10/10 |
| 7 | Follow-on: HUC inference maps (30 HUCs × 2 modes, `huc.txt` batch) | GPU | §10 | 🔄 multiclass ✅, binary running |
| 8 | End-of-reservation sync-back: metrics, weights, GeoTIFFs → `/ibstorage` | CPU | §11 | ⏳ next |
| 9 | Aggregation + viz (code built + validated; run on a full env) | CPU | §8, §12 | ⏳ run after sync-back |

Invariants that hold at every stage: the runner and every follow-on driver are
**idempotent** (completed cells are skipped — stop at a reservation's end, rerun
next session); `/ibstorage/anthony/NYS_Wetlands_DL` is the canonical tree and
the GPU `/workdir` copy is disposable; a fresh `factorial_results_v2/` root
means v1's `factorial_results/` is never touched; Claude prepares scripts,
**the user launches** all GPU/long jobs.

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
| `dl_experiment_config.py` — v2 matrix, dir registry, dir-aware keys (`field_key`/`nwi_field_twin`/`huc12_of`), `LEAKAGE_GUARD`, `--emit --mode` | ✅ done | §10.1 |
| `R_Patches`, `R_Patches_NWI` (692 each, paired 1:1, `MOD_CLASS`) | ✅ on disk, verified | 4.1 |
| `R_Patches_NWIextra` (689 new locations; ∪ NWI = 1381 ≈ 2×) | ✅ on disk, verified | 4.1/8.1 |
| `dl_patch_pools.py` — field-anchored split + dir-aware leakage guard (HUC12 default) | ✅ done, validated | 1.2 / 4.5–4.6 |
| `dl_02_dataset.py` — `create_dataloaders_from_pools()` (resolve_pools + per-config/mode stats) + `label_transform` hook | ✅ done | 1.2 |
| `dl_04_train_lightning.py` — `WetlandPoolsDataModule` + `--config/--mode/--stats-dir/--leakage-guard` (legacy path intact) | ✅ done | 1.2/2.1 |
| `dl_degrade_labels.py` — seeded in-memory train/val degrade (`LabelDegrader` + `make_degrader`), auto-wired into pools for `flddeg` | ✅ done, validated | 1.3 |
| `dl_make_config_stats.py --mode` — all-8 per-config stats both modes incl flddeg; **normalization from the multiclass master for both modes**, **all weights recomputed from disk** (fixes stale field weights + drops the stale binary master) | ✅ done, 16 files built | 1.4 |
| `dl_preflight_check.py` — v2 rewrite: dir presence, off-size flag, predictor parity, field↔NWI pairing+footprint identity, label values/prevalence, 255-mask, split+leakage gate, channels+stats presence | ✅ done, GREEN (0 warnings on the final 692-patch set) | §0 |
| `run_config.sh` — `MODE`/`LEAKAGE_GUARD`/`STATS_DIR`/`DATA_ROOT` knobs, mode-tokened `RESULTS_DIR/<mode>/<config>/seed<k>`, v2 trainer CLI, metrics/manifest from the trainer's field-test journal (no separate dl_05) | ✅ done, dry-run verified | 2.1 |
| `run_factorial.sh` — outer `MODE` loop (2×3×8 = 48 cells, resumable) | ✅ done, dry-run verified | 2.2 |
| wrappers: dropped `run_fld_chm_*.sh`; added `run_nwiextra_*.sh`/`run_nwifield_*.sh`; all 8 loop modes×seeds | ✅ done | 10.4 |
| §11.5 gaps promoted to required: `STATS_DIR` knob (✅ in `run_config.sh`); viz-notebook `EXP_VERSION` root hoist (✅ top cell) — its `<mode>` branch and mode-aware `dl_08` aggregation are the last two gaps | ⏳ see §12 | §11 |

---

## 3. Config & naming (v2)

**8 configs**, each run in **2 modes** × **R seeds** (planned R=3 → 48 runs;
**as run: R=5 → 80 cells**, all complete):

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

# 2. Rebuild the master (only when the R scan / bands / patches changed)  ✅ done
python $PIPE/dl_01_compute_statistics.py \
  --patches-dir  Data/Training_Data/R_Patches \
  --global-stats Data/Training_Data/HUC_DL_Stacks_Extracted_Values.json \
  --weight-power 0.5 \
  --output       Data/Training_Data/multiclass_normalization_stats_wp0.5.json
#   ONE master only: dl_make_config_stats derives BOTH modes' normalization from
#   the multiclass master (binary class weights are recomputed from disk).

# 3. Derive per-config stats from the master, per mode               ✅ done (16 files)
python $PIPE/dl_make_config_stats.py --all --mode multiclass
python $PIPE/dl_make_config_stats.py --all --mode binary

# 4. Preflight — HARD GATE before any GPU time                       ✅ GREEN
python $PIPE/dl_preflight_check.py
#   (v1's --require-all-labels flag no longer exists; v2 flags: --modes/--seeds/
#    --sample/--leakage-guard/--data-root/--stats-dir)
#   v2 asserts: location-key parity (R_Patches ↔ R_Patches_NWI; NWIextra ⊇ NWI),
#   footprint match per shared key, predictor parity, label values {0,1,2,3,255}
#   (binary remap → {0,1,255}), NWI 255-mask == field mask, and the HEADLINE gate:
#   no test_fld key in any config's train/val pool. Must be GREEN for all 8 × 2.
```

> Rebuild the master only when the R scan / bands / patches changed — then rerun
> steps 3–4. (Done 2026-07 over the final 692-patch set; `R_Patches_NWIextra`'s
> extra wetland pixels are already folded into the per-config class weights.)

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

## 6. Launch (inside `tmux`, via `docker1`)   ✅ ready

```bash
tmux new -s factorial_v2
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e EXP_VERSION=v2 \
  -e RESULTS_DIR=/app/Models/factorial_results_v2 \
  nys-wetlands-dl \
  bash Shell_Scripts/run_factorial.sh          # loops MODES="multiclass binary" × configs × seeds
```

**Anatomy of the `docker1` wrapper** — every "GPU node, in-container" command
in this guide (here, §8 SHAP, §10 follow-ons) uses this same template; only the
`-e` knobs and the trailing in-container command change:

```bash
tmux new -s <session>                       # always tmux first; Ctrl-b d detaches
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e <KNOB>=<value> ... \
  nys-wetlands-dl \
  <the in-container command verbatim>
```

- **Env knobs must go through `-e`.** A host-side prefix
  (`MODE=binary docker1 run …`) sets the variable in *your shell*, not in the
  container — the script inside silently falls back to its defaults. One
  `-e NAME=value` per knob; quote lists (`-e SEEDS="0 1 2 3 4"`). This applies
  to every knob in this guide: `MODES`, `CONFIGS`, `SEEDS`, `MODE`,
  `BATCH_SIZE`, `RESULTS_DIR`, `DRY_RUN`, …
- **Verify at launch:** each driver echoes its resolved plan
  (`mode=… seeds=… results=…`) in its first lines of output — check those
  before detaching.
- **Dry run:** add `-e DRY_RUN=1` to the same command to print every resolved
  cell path, stats file, and training command without training anything —
  e.g. confirm cells target `factorial_results_v2/<mode>/…` before burning
  GPU time.
- **Paths in `-e` values are container paths** (`/app/…`, as in the
  `RESULTS_DIR` above), since the script runs inside; only the `-v` mount
  source is a host path.
- Finish one mode's grid before the other (partial-reservation safety): the
  driver's outer loop is `MODES`, so a stopped run still yields a complete
  multiclass factorial. Narrow scope with `-e MODES="multiclass"`,
  `-e CONFIGS="fld_chmret_leafoff nwi_chmret_leafoff"`, `-e SEEDS="0 1 2"`.
- Flag rationale (`--gpus all` / `--shm-size=8g` / `--user` / `-v …:/app` / `--rm`)
  is unchanged from v1 — see `EXECUTION.md` §6.

Each cell writes `Models/factorial_results_v2/<mode>/<config>/seed<k>/`:
`manifest.json` (now records mode, patch dirs, pool rule, leakage regime),
`metrics.json`, `confusion_matrix.csv`, best `.safetensors`/`.ckpt`, logs.

---

## 7. Monitor, resume, sync back

(This is the *mid-run* quick pull for one tree; the full end-of-experiment
checklist — all trees, weights, GeoTIFFs — is §11.)

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

Split mirrors the node split (aggregation CPU, SHAP GPU). **SHAP is done**
(80/80 cells, both modes); aggregation is scoped per mode and runs after
sync-back. The remaining wiring (mode-aware `dl_08`, a cross-mode table, the
`dl_10` `<mode>` branch) is tracked as the punch list in §12.

```bash
# Aggregation — CPU node, after sync-back (SEEDS sets the coverage grid only):
RESULTS_DIR=Models/factorial_results_v2/multiclass SEEDS="0 1 2 3 4" Shell_Scripts/run_aggregate.sh   # ⏳ mode-tokened (§12)
RESULTS_DIR=Models/factorial_results_v2/binary     SEEDS="0 1 2 3 4" Shell_Scripts/run_aggregate.sh
# SHAP — GPU node, in-container, before teardown (backprops through each model).  ✅ done (80/80)
# ✅ dl_09 is v2-aware: it resolves each cell's ACTUAL pools split
# (dl_patch_pools.resolve_pools, same seed + huc12 guard as training -> SHAP
# background = the cell's train pool, SHAP test = its held-out FIELD patches),
# covers ALL 8 configs by default (label-source cells included; narrow with
# --configs), and auto-globs every seed dir present. Run once per mode:
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
  --results-dir Models/factorial_results_v2/multiclass
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) -v /workdir/$USER/nys_wetlands:/app \
  nys-wetlands-dl bash Shell_Scripts/run_shap_factorial.sh \
  --results-dir Models/factorial_results_v2/binary --mode binary
#   --mode picks the mode-tokened stats file; class info comes from each checkpoint.
#   --force only on a RE-run (cells with an existing *_shap_importance.json are
#   skipped otherwise); the v2 tree currently has none, so a first pass needs no flag.
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
3. Land the §10 code (✅ built; commit pending) in one commit so the preflight stays
   the guardrail.
4. Prep (§4) → preflight GREEN for 8 × 2 → `DRY_RUN=1` confirm → user runs (§6).

See the plan §11 for the full assessment of which v1 §11 steps carry over.

---

## 10. Follow-on studies (v2)   ✅ plumbing built

Same node ritual as the base grid: restage (lean push incl. the base grid's
cells for baselines/checkpoints), run in the container under `tmux`, sync back,
aggregate on CPU. All three drivers take a `MODE` knob (default `multiclass`)
and write **new, mode-tokened roots**, so the 80-cell base grid is untouched.
All aggregation reads both metrics.json schemas (v2 nested `test_metrics` / v1
flat).

The bash lines below are what runs *inside* the container — wrap each one in
the `docker1` template from §6 ("Anatomy of the `docker1` wrapper"), passing
knobs like `MODE`/`SEEDS`/`LEVELS` via `-e`. Worked example — arch comparison,
binary mode, all 5 seeds:

```bash
tmux new -s arch_binary
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e MODE=binary -e SEEDS="0 1 2 3 4" \
  nys-wetlands-dl \
  bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff
# expect: "mode=binary" and "seeds: 0 1 2 3 4   (5 cells)" in the header.
# OOM? add -e BATCH_SIZE=4 and rerun — completed seeds are skipped.
```

**Patch-count learning curve** → `Models/results_patchcurve_v2/<mode>/<config>_n<level>/seed<k>/`.
`--n-patches` now works in pools mode: it caps the **TRAIN pool only** (seeded-
shuffle prefix, nested per seed — level 100 ⊂ 200 ⊂ …); **val and the field test
set stay full**, so the curve isolates training-data volume and every level is
scored on exactly the base grid's test patches.

```bash
# GPU node, in-container:
bash Shell_Scripts/run_patchcurve.sh fld_chmret_leafoff      # LEVELS="100 200 300 400 500 full"
# CPU node, after sync-back to Models/results_patchcurve_v2:
python $PIPE/dl_08b_aggregate_patchcurve.py --results-dir Models/results_patchcurve_v2/multiclass
```

**UNet3+ architecture comparison** → `Models/results_arch_v2/<mode>/<config>_unet3plus/seed<k>/`
(deep-supervision ON, bf64/d5 held; `BATCH_SIZE` 8→4 on OOM). The U-Net arm is
the base grid's `Models/factorial_results_v2/<mode>/<config>/` — same seeds,
same pools split, paired by construction.

```bash
# GPU node, in-container:
bash Shell_Scripts/run_arch_compare.sh fld_chmret_leafoff    # SEEDS="0 1 2" (extend to "0 1 2 3 4" for full pairing)
# CPU node, after sync-back:
python $PIPE/dl_08b_aggregate_patchcurve.py --arch-compare --config fld_chmret_leafoff \
  --unet-dir Models/factorial_results_v2/multiclass --unet3plus-dir Models/results_arch_v2/multiclass
```

**HUC prediction / inference maps** → `Data/HUC_DL_Predictions_v2/DLpred_<mode>_cluster_<C>_huc_<H>.tif`
(+ `_probs.tif`). Two-mount pattern and per-HUC source pull are unchanged from
v1 (`EXECUTION.md` §10). Cells resolve from `factorial_results_v2/<MODE>`; the
best-macro-F1 seed is picked from the v2 metrics (nested schema handled).

*Prediction-only reservation (restage after training is done):* prediction needs
NO training patches — `dl_06b_predict_huc.py` assembles the stack in-memory from
the per-HUC source rasters. Per config it needs only the code, the mode-tokened
`stats/`, and each cell's `best_*.safetensors` + `metrics.json` (seed selection)
+ `manifest.json` (arch/bf/depth). So instead of the §5 lean push, from the
**CPU node** ship just (both modes' cells for the config you're mapping with;
~50 MB/cell with `.ckpt` excluded):

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU_NODE=cbsugpu10.biohpc.cornell.edu   # whichever node holds the reservation
ssh $USER@$GPU_NODE 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -avhP --relative \
  --exclude='*.ckpt' --exclude='__pycache__' \
  --exclude='tb_logs' --exclude='lightning_logs' --exclude='shap' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts \
  Data/Training_Data/stats \
  Models/factorial_results_v2/multiclass/fld_chmret_leafoff \
  Models/factorial_results_v2/binary/fld_chmret_leafoff \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

Always restage even if `/workdir/$USER/nys_wetlands` survived the previous
reservation (stale wrappers otherwise), and check the image is still loaded
(`docker1 images`; if wiped, re-`scp` the tarball and `docker1 load -i` per
`EXECUTION.md` §4). Then pull sources and predict — one run per mode; the
`DLpred_<mode>_…` naming keeps both in the same output dir:

```bash
SERVER=… REMOTE_ROOT=… LOCAL_ROOT=/workdir/$USER/NYS_Wetlands_Data \
  bash Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>
DATA_ROOT=/workdir/$USER/NYS_Wetlands_Data bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>
MODE=binary DATA_ROOT=… bash Shell_Scripts/run_predict_factorial.sh fld_chmret_leafoff <cluster> <huc>
```

*Batch over many HUCs:* list them in `Shell_Scripts/huc.txt`, one `<cluster>:<huc>`
per line (blank lines ignored), then loop inside a single two-mount container
(in `tmux`; add `-e MODE=binary` for the binary model, `-e DRY_RUN=1` to
resolve checkpoints/stats without predicting):

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

The `|| echo [FAILED]` keeps the loop going past a HUC with missing source
tiles — grep the log/scrollback for `FAILED` afterwards. Source rasters for
every listed HUC must already be pulled via `rsync_huc_sources.sh`.

---

## 11. End-of-reservation sync-back (everything)

Everything below runs **FROM the CPU node**. `/workdir` on the GPU node is
local, unshared, and not backed up — treat this section as the checklist to
drain a reservation before teardown. `/ibstorage/anthony/NYS_Wetlands_DL`
stays the canonical source of truth. All rsync passes are idempotent: safe to
run mid-training (e.g. while HUC prediction is still looping) and re-run to
catch up.

```bash
cd /ibstorage/anthony/NYS_Wetlands_DL
GPU="$USER@cbsugpu09.biohpc.cornell.edu"   # or cbsugpu10 — whichever held the reservation
WD="/workdir/$USER/nys_wetlands"
```

**1 · Base factorial grid** (§7; both modes ride along — the root is mode-tokened).
SHAP JSON/PNG live inside the cells, so they come with `--metrics-only`:

```bash
SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/factorial_results_v2" \
LOCAL_DEST=Models/factorial_results_v2 \
  Shell_Scripts/rsync_results.sh --metrics-only     # -n first to preview
```

**2 · Follow-on result trees** (same script, different roots):

```bash
for tree in results_patchcurve_v2 results_arch_v2; do
  SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/$tree" LOCAL_DEST="Models/$tree" \
    Shell_Scripts/rsync_results.sh --metrics-only
done
```

**3 · Model weights** (once, before teardown). `--metrics-only` excludes
`.ckpt`/`.safetensors`; the deployable checkpoints must land on `/ibstorage`
or they die with the node copy. Rerun 1–2 **without** the flag — already-synced
metrics are skipped, only weights move (or add `--exclude '*.ckpt'` via
`RSYNC_OPTS` to keep just the safetensors, ~50 MB/cell vs ~500 MB):

```bash
SERVER="$GPU:" REMOTE_RESULTS="$WD/Models/factorial_results_v2" \
LOCAL_DEST=Models/factorial_results_v2 \
RSYNC_OPTS="-avz --progress --exclude=*.ckpt" \
  Shell_Scripts/rsync_results.sh
# repeat for results_patchcurve_v2 / results_arch_v2 if those weights are wanted
```

**4 · HUC prediction GeoTIFFs** (plain rsync — flat dir, no config/seed layout;
multi-GB, gitignored, feeds `dl_10b_huc_inference_viz.ipynb`):

```bash
rsync -avhP "$GPU:$WD/Data/HUC_DL_Predictions_v2/" Data/HUC_DL_Predictions_v2/
```

**5 · Close the loop on CPU:** aggregate (§8 per mode), rerun the notebook(s),
then `git add -A` so the whitelisted analysis JSON/CSV sync to the local Mac
via git. The aggregation/viz code still needs the §12 fixes first.

---

## 12. Remaining analysis work

The CPU-side analysis code is now **built and validated** (2026-07-09); what's
left is to run it on a full env after sync-back. Two v2 schema facts drove the
fixes, and any future reader of a v2 `metrics.json` must respect both:

> **Gotcha 1 — nested scores.** `run_config.sh` writes the score block **nested
> under `"test_metrics"`** (`overall_accuracy`/`mean_iou`/`macro_f1`/`per_class`);
> v1's `dl_05` wrote those flat at top level. Unwrap with
> `scores = metrics.get("test_metrics") or metrics`, then read from `scores`.
> (v2 also drops `macro_recall`/`macro_precision` from that block — recover them
> as the unweighted class mean of `per_class`, matching how `macro_f1` is defined.)
>
> **Gotcha 2 — confusion matrix is a dict.** v2 stores `confusion_matrix` as
> `{"labels": [...], "matrix": [[...]]}` at the **top level** (not nested), where
> v1 stored a **bare nested list**. `np.array(cm)` on the v2 dict raises. Parse
> with: if `isinstance(cm, dict)` use `cm["matrix"]` + `cm["labels"]`, else the
> bare list with labels from `class_names`/`per_class`.

Status of the punch list:

1. **`dl_08_aggregate_factorial.py` — ✅ done.** `load_cells()` unwraps
   `_scores()` (Gotcha 1) and recovers the two macros; `mean_confusion_matrices()`
   goes through a new `_confusion()` helper handling both matrix forms (Gotcha 2).
   Validated: 80/80 cells produce complete scalars, labels intact, both modes.
   Run per mode via `run_aggregate.sh` (§8) after sync-back → CSVs land in
   `factorial_results_v2/<mode>/analysis/`.
2. **`dl_10_factorial_viz.ipynb` — ✅ done.** Setup cell now has a `MODE`
   selector (env `MODE`, default `multiclass`) that resolves
   `factorial_results_v2/<mode>/analysis` and a `with_mode()` helper that carries
   the level to the patch-curve / arch roots; `CONFIG_ORDER` is derived from
   `dl_experiment_config.CONFIGS` (hardcoded v2 fallback if the import is
   unavailable); `CLASS_ORDER` branches to `WET/UPL` in binary mode; and the
   patch-curve (§6) and arch (§7) loaders share the `_scores()` unwrap.
3. **Cross-mode comparison — ✅ built (first cut).** New notebook **§9** reads
   *both* modes' aggregated outputs and writes
   `factorial_results_v2/analysis/cross_mode_summary.csv` + figures. Two
   apples-to-apples views: **UPL** (identical class both modes → mean ± sd over
   seeds) and **WET** (collapse each multiclass model's seed-mean confusion
   matrix EMW/FSW/SSW→WET and compare to the native-binary WET — the fair
   "collapse a 4-class model vs train binary" baseline). Includes the
   label-gradient panel (`nwi → nwiextra → nwifield → flddeg → fld`) in both
   modes. Macro-F1 is deliberately *not* compared across modes (different class
   counts). Refine the spec (e.g. add per-seed WET sd via per-seed CMs) as needed.

**To produce the figures** (CPU, after §11 sync-back, on an env with rasterio +
seaborn — the uv `nys-wetlands-dl` or conda `wetland-cnn` kernel):
aggregate both modes (§8) → run `dl_10` with `MODE=multiclass`, then again with
`MODE=binary` (§9 reads both regardless) → run `dl_10b` on the synced HUC
GeoTIFFs → `git add -A` to ship the analysis JSON/CSV to the Mac.

---

## Pointers
- **Flow map + live status:** §0 (the one table to read first).
- **Design / rationale / decisions:** `wetland_factorial_experiment_plan_v2.md`
  (its §5 "Implementation phases" now reflects the completed run; Phase 3 there
  matches §12 here).
- **v1 mechanics that still apply verbatim:** `EXECUTION.md` §1 (node split), §4
  (Docker build/load), §6 (docker flags), §7 (TensorBoard) — read via the tag if
  the working copy has drifted.
- **Follow-on studies (patch curve / UNet3+ / HUC inference):** §10 above for
  the v2 commands; design in plan §9.
- **What's left to build:** §12 (aggregation/viz punch list).
- **Draining a reservation:** §11 (full sync-back checklist).
