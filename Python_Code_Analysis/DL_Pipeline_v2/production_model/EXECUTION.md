# Production Model — Execution Guide

Operational walkthrough for training the single deployable model. The node ritual
is **identical** to the factorial's (`../factorial_experiment/EXECUTION_v2.md`) —
this guide only states what differs. Read that one first if the two-node split is
unfamiliar.

> **Agent boundary:** Claude prepares scripts; **you run** all GPU jobs, containers,
> and rsync. Nothing here auto-launches.

---

## 0. What differs from a factorial run

| | Factorial | Production |
|---|---|---|
| driver | `run_factorial.sh` | `run_production.sh` |
| recipe | 8 configs × 2 modes × 3 seeds | 1 config × 1 mode × 3 seeds (`dl_prod_config.py`) |
| results root | `Models/factorial_results_v2` | `Models/production_model` |
| cell dir | `<mode>/<config>/seed<k>` | `<mode>/production/seed<k>` |
| epochs | 50 | 100 |

Everything else — stats files, pools, leakage guard, field-labeled test set,
metrics/manifest extraction — is the same code path (`run_production.sh` is a thin
wrapper over `run_config.sh`).

---

## 1. CPU prep

```bash
conda activate wetland-cnn          # (there is no .venv on the BioHPC CPU node)
cd /ibstorage/anthony/NYS_Wetlands_DL
PIPE=Python_Code_Analysis/DL_Pipeline_v2

python $PIPE/production_model/dl_prod_config.py     # print + self-check the recipe
DRY_RUN=1 bash Shell_Scripts/run_production.sh      # print commands, train nothing
```

The dry run must show `results: .../Models/production_model/multiclass/production/seed<k>/`.
If it shows `factorial_results_v2`, stop — the wrapper is not being used.

**Stats must already exist.** Production reuses the factorial's per-config stats
(`Data/Training_Data/stats/multiclass_normalization_stats_nwifield_chmret_leafoff_wp0.5.json`
for training, the `fld_chmret_leafoff` file for eval). They were built in the
factorial prep; rebuild only if bands/patches changed:

```bash
python $PIPE/dl_make_config_stats.py --all --mode multiclass
```

Preflight is likewise already GREEN for this config from the factorial run — rerun
it if the patch directories have changed since.

---

## 2. Stage onto `/workdir` (from the CPU node)

Same lean push as the factorial; production needs `R_Patches` + `R_Patches_NWIextra`
(the `nwifield` hybrid pool):

```bash
GPU_NODE=cbsugpu10.biohpc.cornell.edu
ssh $USER@$GPU_NODE 'mkdir -p /workdir/$USER/nys_wetlands /workdir/$USER/tmp'
rsync -avhP --relative --exclude='*.ckpt' --exclude='__pycache__' \
  Python_Code_Analysis/DL_Pipeline_v2 Shell_Scripts \
  Data/Training_Data/stats \
  Data/Training_Data/R_Patches Data/Training_Data/R_Patches_NWIextra \
  "$USER@$GPU_NODE:/workdir/$USER/nys_wetlands/"
```

Load the image as usual (`docker1 load -i /workdir/$USER/nys-wetlands-dl.tar.gz`).

---

## 3. Launch (GPU node, inside `tmux`, via `docker1`)

```bash
tmux new -s production
docker1 run --rm --gpus all --shm-size=8g --user $(id -u):$(id -g) \
  -v /workdir/$USER/nys_wetlands:/app -e TMPDIR=/app/tmp \
  -e RESULTS_DIR=/app/Models/production_model \
  nys-wetlands-dl \
  bash Shell_Scripts/run_production.sh
```

- **Knobs must go through `-e`** — a host-side prefix sets the variable in *your*
  shell, not the container. Same trap as the factorial.
- **Dry run first:** add `-e DRY_RUN=1` and confirm the resolved paths before
  burning GPU time.
- **One seed at a time** if the reservation is short: `run_production.sh 0` (a
  positional arg overrides the recipe's seed list). Completed seeds are skipped on
  rerun, so stop/resume is safe.
- **OOM?** `-e BATCH_SIZE=8`. Note this departs from the held factorial schedule —
  record it if you do.

Verify the header before detaching (Ctrl-b d): it echoes config, mode, arch,
schedule, seeds, and results root.

---

## 4. Sync back (from the CPU node)

Metrics only:

```bash
SERVER="$USER@cbsugpu10.biohpc.cornell.edu:" \
REMOTE_RESULTS="/workdir/$USER/nys_wetlands/Models/production_model" \
LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/production_model" \
  Shell_Scripts/rsync_results.sh --metrics-only     # -n to preview
```

**The checkpoint is the deliverable here** — unlike the factorial, you *do* want the
weights. Pull them explicitly:

```bash
rsync -avhP \
  "$USER@cbsugpu10.biohpc.cornell.edu:/workdir/$USER/nys_wetlands/Models/production_model/" \
  Models/production_model/
```

`.gitignore` tracks `metrics.json` / `manifest.json` / `training_log.json` under
`Models/production_model/` and ignores the `.ckpt`/`.safetensors` — so the scores
sync via git while the weights stay out of history. Run `git add -A` afterwards.

---

## 5. Pick the shipped model

```bash
python - <<'PY'
import json, glob
for p in sorted(glob.glob("Models/production_model/*/production/seed*/metrics.json")):
    m = json.load(open(p))["test_metrics"]
    print(f"{p:60s} macroF1={m.get('macro_f1'):.4f}")
PY
```

Ship the best macro-F1 seed's `best_*.safetensors` (+ its `.meta.json` sidecar,
which carries the architecture for auto-detection on load). See PLAN.md §4 for the
single-vs-ensemble and full-pool-refit decisions, both still open.

---

## 6. Inference

Unchanged from the factorial path — `Shell_Scripts/run_predict_factorial.sh` with
`RESULTS_DIR` pointed at `Models/production_model`, then `python_make_cogs.py` and
the `webmap/` viewer. Architecture is auto-detected from the checkpoint, so no
`--arch` is needed.

---

## Pointers

- Recipe / source of truth: `dl_prod_config.py`
- Rationale + open decisions: `PLAN.md`
- Node ritual in full: `../factorial_experiment/EXECUTION_v2.md`
- Branching policy: root `CLAUDE.md` § "Repo & branching"
