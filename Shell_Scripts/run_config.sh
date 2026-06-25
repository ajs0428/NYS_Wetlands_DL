#!/usr/bin/env bash
# run_config.sh -- train + evaluate ONE factorial cell (one config x one seed).
#
# Phase 2.1 of the wetland factorial experiment. Holds architecture, loss,
# optimizer, schedule, and weight_power constant across every run; only the
# active predictor bands and the training label source change (encoded in the
# per-config stats file). The test set is ALWAYS field-labeled (Section 3): a
# config trains on its own label source but is evaluated with the matching
# fld_<featureset> stats, so nwi/flddeg are scored against field truth on the
# same seed-determined test patches.
#
# Idempotent (Phase 2.2a): a cell whose results/<config>/seed<k>/metrics.json +
# manifest.json already exist is skipped, so the factorial survives stop/resume
# across BioHPC reservation windows.
#
# Usage:   run_config.sh <config> <seed>
# Example: run_config.sh fld_chm_leafon 0
#
# Knobs (env overrides): EPOCHS BATCH_SIZE BASE_FILTERS DEPTH PRECISION
#   PATCHES_DIR RESULTS_DIR PYTHON DRY_RUN
#
# Follow-on-study knobs (default to base-factorial behavior when unset, so the
# 8x3 factorial is unchanged):
#   ARCH=unet|unet3plus      architecture (default unet)
#   CAT_CHANNELS=64          [unet3plus] unified skip-branch channels
#   DEEP_SUPERVISION=0|1     [unet3plus] add per-stage loss heads (default 0)
#   N_PATCHES=<int>          cap the patch pool for learning-curve runs (default: all)
#   CELL_NAME=<dir>          cell directory name under RESULTS_DIR (default: $CONFIG).
#                            Stats still resolve from the real $CONFIG, so studies
#                            can write to e.g. <config>_n200 / <config>_unet3plus
#                            without colliding with the base results/ tree.
set -euo pipefail

CONFIG="${1:?usage: run_config.sh <config> <seed>}"
SEED="${2:?usage: run_config.sh <config> <seed>}"

# --- Paths (resolved from this script's location -> repo root) ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"
DATA="$REPO_ROOT/Data/Training_Data"
STATS_DIR="$DATA/stats"
PATCHES_DIR="${PATCHES_DIR:-$DATA/R_Patches_Merged}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/results}"
PYTHON="${PYTHON:-python}"

# --- Fixed experiment constants (do NOT vary across the base factorial) ---
# Loss/weighting stay fixed for every study. ARCH is env-overridable for the
# architecture follow-on (default unet preserves base-factorial behavior).
ARCH="${ARCH:-unet}"
CE_WEIGHT="1.0"; DICE_WEIGHT="0.0"; FOCAL_GAMMA="0"   # plain weighted CE
BASE_FILTERS="${BASE_FILTERS:-64}"
DEPTH="${DEPTH:-5}"
EPOCHS="${EPOCHS:-50}"
BATCH_SIZE="${BATCH_SIZE:-16}"
PRECISION="${PRECISION:-16-mixed}"

# --- Follow-on-study knobs (default to base-factorial behavior) ---
CAT_CHANNELS="${CAT_CHANNELS:-64}"          # [unet3plus] skip-branch channels
DEEP_SUPERVISION="${DEEP_SUPERVISION:-0}"   # [unet3plus] per-stage loss heads
N_PATCHES="${N_PATCHES:-}"                   # learning-curve patch cap (empty = all)

# --- Resolve config -> stats files (single source of truth: dl_experiment_config) ---
eval "$("$PYTHON" "$PIPE/dl_experiment_config.py" --emit "$CONFIG")"
TRAIN_STATS_PATH="$STATS_DIR/$TRAIN_STATS"
EVAL_STATS_PATH="$STATS_DIR/$EVAL_STATS"
# Cell dir name defaults to the config; studies override it (e.g. <config>_n200)
# while stats still resolve from the real $CONFIG above.
CELL_NAME="${CELL_NAME:-$CONFIG}"
CELL="$RESULTS_DIR/$CELL_NAME/seed$SEED"

echo "=============================================================="
echo " cell:      $CELL_NAME / seed$SEED   (config: $CONFIG)"
echo " arch:      $ARCH   bf$BASE_FILTERS d$DEPTH${N_PATCHES:+   n_patches: $N_PATCHES}"
echo " label src: $LABEL_SOURCE   in_channels: $IN_CHANNELS"
echo " train stats: $TRAIN_STATS"
echo " eval  stats: $EVAL_STATS   (eval config: $EVAL_CONFIG, field-labeled)"
echo " out:       $CELL"
echo "=============================================================="

# --- Idempotency: skip a completed cell. ---
if [[ -f "$CELL/metrics.json" && -f "$CELL/manifest.json" ]]; then
    echo "[skip] already complete (metrics.json + manifest.json present)."
    exit 0
fi

for f in "$TRAIN_STATS_PATH" "$EVAL_STATS_PATH"; do
    [[ -f "$f" ]] || { echo "[error] missing stats: $f"; echo "  run: python $PIPE/dl_make_config_stats.py --all"; exit 1; }
done
[[ -d "$PATCHES_DIR" ]] || { echo "[error] missing patches dir: $PATCHES_DIR"; exit 1; }

run() { echo "+ $*"; [[ "${DRY_RUN:-0}" == "1" ]] || "$@"; }

# In a dry run, don't touch the tree: send logs to /dev/null and skip mkdir.
if [[ "${DRY_RUN:-0}" == "1" ]]; then
    LOG_TRAIN=/dev/null; LOG_EVAL=/dev/null
else
    mkdir -p "$CELL"; LOG_TRAIN="$CELL/train.log"; LOG_EVAL="$CELL/eval.log"
fi

# --- Optional train flags (only set when a study requests them). ---
EXTRA_TRAIN_ARGS=()
[[ -n "$N_PATCHES" ]] && EXTRA_TRAIN_ARGS+=(--n-patches "$N_PATCHES")
if [[ "$ARCH" == "unet3plus" ]]; then
    EXTRA_TRAIN_ARGS+=(--cat-channels "$CAT_CHANNELS")
    [[ "$DEEP_SUPERVISION" == "1" ]] && EXTRA_TRAIN_ARGS+=(--deep-supervision)
fi

# --- 1. Train (validation follows the training label source). ---
run "$PYTHON" "$PIPE/dl_04_train_lightning.py" \
    --patches-dir "$PATCHES_DIR" \
    --stats-path "$TRAIN_STATS_PATH" \
    --output-dir "$CELL" \
    --seed "$SEED" \
    --epochs "$EPOCHS" --batch-size "$BATCH_SIZE" \
    --base-filters "$BASE_FILTERS" --depth "$DEPTH" \
    --arch "$ARCH" --precision "$PRECISION" \
    --ce-weight "$CE_WEIGHT" --dice-weight "$DICE_WEIGHT" --focal-gamma "$FOCAL_GAMMA" \
    ${EXTRA_TRAIN_ARGS[@]+"${EXTRA_TRAIN_ARGS[@]}"} \
    2>&1 | tee "$LOG_TRAIN"

# --- 2. Locate the best checkpoint (prefer self-describing safetensors). ---
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    CKPT="$(ls -t "$CELL"/best_*.safetensors 2>/dev/null | head -1 || true)"
    [[ -z "$CKPT" ]] && CKPT="$(ls -t "$CELL"/best_*.ckpt 2>/dev/null | head -1 || true)"
    [[ -n "$CKPT" ]] || { echo "[error] no best_* checkpoint in $CELL"; exit 1; }
else
    CKPT="$CELL/best_<dry-run>.safetensors"
fi
echo "checkpoint: $CKPT"

# --- 3. Evaluate on the FIELD test set (same seed -> same test patches). ---
run "$PYTHON" "$PIPE/dl_05_evaluate.py" \
    --model "$CKPT" \
    --patches-dir "$PATCHES_DIR" \
    --stats-path "$EVAL_STATS_PATH" \
    --seed "$SEED" \
    --output "$CELL/metrics.json" \
    2>&1 | tee "$LOG_EVAL"

# --- 4. Confusion-matrix CSV + run manifest (Phase 2.3). ---
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    GIT_COMMIT="$(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"
    CONFIG="$CONFIG" SEED="$SEED" CELL="$CELL" CELL_NAME="$CELL_NAME" LABEL_SOURCE="$LABEL_SOURCE" \
    IN_CHANNELS="$IN_CHANNELS" TRAIN_STATS_PATH="$TRAIN_STATS_PATH" \
    EVAL_STATS_PATH="$EVAL_STATS_PATH" EVAL_CONFIG="$EVAL_CONFIG" CKPT="$CKPT" \
    ARCH="$ARCH" BASE_FILTERS="$BASE_FILTERS" DEPTH="$DEPTH" EPOCHS="$EPOCHS" \
    BATCH_SIZE="$BATCH_SIZE" PRECISION="$PRECISION" CE_WEIGHT="$CE_WEIGHT" \
    DICE_WEIGHT="$DICE_WEIGHT" FOCAL_GAMMA="$FOCAL_GAMMA" GIT_COMMIT="$GIT_COMMIT" \
    PATCHES_DIR="$PATCHES_DIR" N_PATCHES="$N_PATCHES" \
    CAT_CHANNELS="$CAT_CHANNELS" DEEP_SUPERVISION="$DEEP_SUPERVISION" \
    "$PYTHON" - <<'PY'
import json, os
from pathlib import Path

cell = Path(os.environ["CELL"])
metrics = json.loads((cell / "metrics.json").read_text())

# confusion_matrix.csv (true rows x predicted cols)
cm = metrics.get("confusion_matrix")
if cm is not None:
    import csv
    with open(cell / "confusion_matrix.csv", "w", newline="") as f:
        csv.writer(f).writerows(cm)

train_stats = json.loads(Path(os.environ["TRAIN_STATS_PATH"]).read_text())

n_patches_env = os.environ.get("N_PATCHES", "")
manifest = {
    "config": os.environ["CONFIG"],
    "cell_name": os.environ.get("CELL_NAME", os.environ["CONFIG"]),
    "seed": int(os.environ["SEED"]),
    "label_source": os.environ["LABEL_SOURCE"],
    "in_channels": int(os.environ["IN_CHANNELS"]),
    "predictor_names": train_stats.get("predictor_names"),
    "ignore_index": train_stats.get("ignore_index", 255),
    "weight_power": train_stats.get("weight_power"),
    "class_weights": train_stats.get("class_weights"),
    "loss": {"ce_weight": float(os.environ["CE_WEIGHT"]),
             "dice_weight": float(os.environ["DICE_WEIGHT"]),
             "focal_gamma": float(os.environ["FOCAL_GAMMA"])},
    "arch": os.environ["ARCH"],
    "base_filters": int(os.environ["BASE_FILTERS"]),
    "depth": int(os.environ["DEPTH"]),
    "cat_channels": int(os.environ["CAT_CHANNELS"]) if os.environ["ARCH"] == "unet3plus" else None,
    "deep_supervision": os.environ.get("DEEP_SUPERVISION") == "1" if os.environ["ARCH"] == "unet3plus" else None,
    "n_patches": int(n_patches_env) if n_patches_env else None,
    "epochs": int(os.environ["EPOCHS"]),
    "batch_size": int(os.environ["BATCH_SIZE"]),
    "precision": os.environ["PRECISION"],
    "train_stats": Path(os.environ["TRAIN_STATS_PATH"]).name,
    "eval_stats": Path(os.environ["EVAL_STATS_PATH"]).name,
    "eval_config": os.environ["EVAL_CONFIG"],
    "checkpoint": Path(os.environ["CKPT"]).name,
    "patches_dir": os.environ["PATCHES_DIR"],
    "git_commit": os.environ["GIT_COMMIT"],
}

# flddeg provenance: fold in the degrade manifest (seed + achieved prevalence).
deg = Path(os.environ["PATCHES_DIR"]) / "degrade_manifest.json"
if os.environ["LABEL_SOURCE"] == "flddeg" and deg.exists():
    manifest["degrade"] = json.loads(deg.read_text())

(cell / "manifest.json").write_text(json.dumps(manifest, indent=2))
print(f"wrote {cell/'manifest.json'} and confusion_matrix.csv")
PY
fi

echo "[done] $CONFIG / seed$SEED"
