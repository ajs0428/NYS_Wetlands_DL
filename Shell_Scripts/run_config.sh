#!/usr/bin/env bash
# run_config.sh -- train + evaluate ONE factorial-v2 cell (config x seed x mode).
#
# Phase 2.1. Holds architecture, loss, optimizer, schedule, and weight_power
# constant across every run; only the active predictor bands, the training label
# source, and the classification MODE change (all encoded in the per-config,
# per-mode stats file + the field-anchored pools).
#
# v2 data flow (differs from v1): data comes from dl_patch_pools.resolve_pools,
# NOT a single --patches-dir. The trainer takes --config/--mode/--stats-dir/
# --leakage-guard and ALREADY evaluates on the field test set (test = R_Patches at
# the seed's held-out footprints, identical across configs), writing test metrics
# to <cell>/training_log.json -- so there is no separate dl_05 eval step; this
# script extracts metrics.json + confusion_matrix.csv + manifest.json from it.
#
# Idempotent: a cell whose metrics.json + manifest.json exist is skipped, so the
# grid survives stop/resume across BioHPC reservation windows.
#
# Usage:   run_config.sh <config> <seed>
# Example: MODE=binary run_config.sh nwiextra_chmret_leafoff 0
#
# Knobs (env overrides): MODE(multiclass|binary) LEAKAGE_GUARD(huc12|coord)
#   EPOCHS BATCH_SIZE BASE_FILTERS DEPTH PRECISION
#   STATS_DIR DATA_ROOT RESULTS_DIR PYTHON DRY_RUN
#   Results root is mode-tokened: RESULTS_DIR/<mode>/<config>/seed<k>/.
#
# Follow-on-study knobs (default to base-factorial behavior when unset):
#   ARCH=unet|unet3plus  CAT_CHANNELS=64  DEEP_SUPERVISION=0|1
#   N_PATCHES=<n>        learning curve: cap the TRAIN pool to n (seeded-shuffle
#                        prefix, nested per seed); val/test stay full.
#   CELL_NAME=<dir>      cell dir under RESULTS_DIR/<mode> (default: $CONFIG);
#                        stats still resolve from the real $CONFIG.
set -euo pipefail

CONFIG="${1:?usage: run_config.sh <config> <seed>}"
SEED="${2:?usage: run_config.sh <config> <seed>}"

# --- Paths (resolved from this script's location -> repo root) ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"
DATA="$REPO_ROOT/Data/Training_Data"
DATA_ROOT="${DATA_ROOT:-$DATA}"
STATS_DIR="${STATS_DIR:-$DATA/stats}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/Models/factorial_results_v2}"
PYTHON="${PYTHON:-python}"

# --- Mode + leakage regime (v2 axes) ---
MODE="${MODE:-multiclass}"
LEAKAGE_GUARD="${LEAKAGE_GUARD:-huc12}"

# --- Fixed experiment constants (do NOT vary across the base factorial) ---
ARCH="${ARCH:-unet}"
CE_WEIGHT="1.0"; DICE_WEIGHT="0.0"; FOCAL_GAMMA="0"   # plain weighted CE
BASE_FILTERS="${BASE_FILTERS:-64}"
DEPTH="${DEPTH:-5}"
EPOCHS="${EPOCHS:-50}"
BATCH_SIZE="${BATCH_SIZE:-16}"
PRECISION="${PRECISION:-16-mixed}"

# --- Follow-on-study knobs (default to base-factorial behavior) ---
CAT_CHANNELS="${CAT_CHANNELS:-64}"
DEEP_SUPERVISION="${DEEP_SUPERVISION:-0}"

# --- Resolve config -> stats files (single source of truth: dl_experiment_config) ---
eval "$("$PYTHON" "$PIPE/dl_experiment_config.py" --emit "$CONFIG" --mode "$MODE")"
TRAIN_STATS_PATH="$STATS_DIR/$TRAIN_STATS"
EVAL_STATS_PATH="$STATS_DIR/$EVAL_STATS"
CELL_NAME="${CELL_NAME:-$CONFIG}"
CELL="$RESULTS_DIR/$MODE/$CELL_NAME/seed$SEED"

echo "=============================================================="
echo " cell:      $MODE / $CELL_NAME / seed$SEED   (config: $CONFIG)"
echo " arch:      $ARCH   bf$BASE_FILTERS d$DEPTH"
echo " label src: $LABEL_SOURCE   pool: $POOL_RULE   guard: $LEAKAGE_GUARD"
echo " patch dirs:$PATCH_DIRS   in_channels: $IN_CHANNELS"
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
    [[ -f "$f" ]] || { echo "[error] missing stats: $f"; echo "  run: python $PIPE/dl_make_config_stats.py --all --mode $MODE"; exit 1; }
done
[[ -d "$DATA_ROOT/R_Patches" ]] || { echo "[error] missing field dir: $DATA_ROOT/R_Patches"; exit 1; }

run() { echo "+ $*"; [[ "${DRY_RUN:-0}" == "1" ]] || "$@"; }

if [[ "${DRY_RUN:-0}" == "1" ]]; then
    LOG_TRAIN=/dev/null
else
    mkdir -p "$CELL"; LOG_TRAIN="$CELL/train.log"
fi

# --- Optional train flags (only set when a study requests them). ---
EXTRA_TRAIN_ARGS=()
if [[ "$ARCH" == "unet3plus" ]]; then
    EXTRA_TRAIN_ARGS+=(--cat-channels "$CAT_CHANNELS")
    [[ "$DEEP_SUPERVISION" == "1" ]] && EXTRA_TRAIN_ARGS+=(--deep-supervision)
fi
[[ -n "${N_PATCHES:-}" ]] && EXTRA_TRAIN_ARGS+=(--n-patches "$N_PATCHES")

# --- Train + field-test in one shot (trainer resolves pools + evaluates on field). ---
run "$PYTHON" "$PIPE/dl_04_train_lightning.py" \
    --config "$CONFIG" --seed "$SEED" --mode "$MODE" \
    --stats-dir "$STATS_DIR" --data-root "$DATA_ROOT" \
    --leakage-guard "$LEAKAGE_GUARD" \
    --output-dir "$CELL" \
    --epochs "$EPOCHS" --batch-size "$BATCH_SIZE" \
    --base-filters "$BASE_FILTERS" --depth "$DEPTH" \
    --arch "$ARCH" --precision "$PRECISION" \
    --ce-weight "$CE_WEIGHT" --dice-weight "$DICE_WEIGHT" --focal-gamma "$FOCAL_GAMMA" \
    ${EXTRA_TRAIN_ARGS[@]+"${EXTRA_TRAIN_ARGS[@]}"} \
    2>&1 | tee "$LOG_TRAIN"

# --- Extract metrics.json + confusion_matrix.csv + manifest.json from the
#     trainer's field-test journal (last entry of training_log.json). ---
if [[ "${DRY_RUN:-0}" != "1" ]]; then
    CKPT="$(ls -t "$CELL"/best_*.safetensors 2>/dev/null | head -1 || true)"
    [[ -z "$CKPT" ]] && CKPT="$(ls -t "$CELL"/best_*.ckpt 2>/dev/null | head -1 || true)"
    GIT_COMMIT="$(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"

    CONFIG="$CONFIG" SEED="$SEED" MODE="$MODE" CELL="$CELL" CELL_NAME="$CELL_NAME" \
    LABEL_SOURCE="$LABEL_SOURCE" POOL_RULE="$POOL_RULE" LEAKAGE_GUARD="$LEAKAGE_GUARD" \
    IN_CHANNELS="$IN_CHANNELS" PATCH_DIRS="$PATCH_DIRS" \
    TRAIN_STATS_PATH="$TRAIN_STATS_PATH" EVAL_STATS_PATH="$EVAL_STATS_PATH" \
    EVAL_CONFIG="$EVAL_CONFIG" CKPT="${CKPT:-}" ARCH="$ARCH" \
    BASE_FILTERS="$BASE_FILTERS" DEPTH="$DEPTH" EPOCHS="$EPOCHS" \
    BATCH_SIZE="$BATCH_SIZE" PRECISION="$PRECISION" CE_WEIGHT="$CE_WEIGHT" \
    DICE_WEIGHT="$DICE_WEIGHT" FOCAL_GAMMA="$FOCAL_GAMMA" GIT_COMMIT="$GIT_COMMIT" \
    DATA_ROOT="$DATA_ROOT" CAT_CHANNELS="$CAT_CHANNELS" DEEP_SUPERVISION="$DEEP_SUPERVISION" \
    N_PATCHES="${N_PATCHES:-}" \
    "$PYTHON" - <<'PY'
import json, os, csv, re
from pathlib import Path

cell = Path(os.environ["CELL"])
journal = json.loads((cell / "training_log.json").read_text())
entry = journal[-1]  # this run's train+field-test record

test = entry.get("test_metrics", {})
cm = entry.get("confusion_matrix", {})
class_names = entry.get("config", {}).get("class_names", (cm.get("labels") or []))

# metrics.json -- what the aggregation reads (mode-tagged, field-test scores).
metrics = {
    "config": os.environ["CONFIG"],
    "mode": os.environ["MODE"],
    "seed": int(os.environ["SEED"]),
    "class_names": class_names,
    "test_metrics": test,
    "confusion_matrix": cm,
    "best_val_loss": entry.get("best_val_loss"),
    "best_epoch": entry.get("best_epoch"),
    "checkpoint": Path(os.environ["CKPT"]).name if os.environ.get("CKPT") else None,
}
(cell / "metrics.json").write_text(json.dumps(metrics, indent=2))

matrix = cm.get("matrix")
if matrix is not None:
    with open(cell / "confusion_matrix.csv", "w", newline="") as f:
        csv.writer(f).writerows(matrix)

train_stats = json.loads(Path(os.environ["TRAIN_STATS_PATH"]).read_text())

# Best-effort degrade provenance for flddeg: pull the flip_prob the degrader
# logged (in-memory degrade, so it lives in train.log, not a manifest file).
degrade = None
if os.environ["LABEL_SOURCE"] == "flddeg":
    log = cell / "train.log"
    degrade = {"applied": True}
    if log.exists():
        m = re.search(r"flip_prob=([0-9.]+)", log.read_text())
        if m:
            degrade["flip_prob"] = float(m.group(1))

manifest = {
    "config": os.environ["CONFIG"],
    "mode": os.environ["MODE"],
    "cell_name": os.environ.get("CELL_NAME", os.environ["CONFIG"]),
    "seed": int(os.environ["SEED"]),
    "label_source": os.environ["LABEL_SOURCE"],
    "pool_rule": os.environ["POOL_RULE"],
    "leakage_guard": os.environ["LEAKAGE_GUARD"],
    "patch_dirs": os.environ["PATCH_DIRS"].split(),
    "in_channels": int(os.environ["IN_CHANNELS"]),
    "predictor_names": train_stats.get("predictor_names"),
    "class_names": train_stats.get("class_names"),
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
    "epochs": int(os.environ["EPOCHS"]),
    "batch_size": int(os.environ["BATCH_SIZE"]),
    "n_patches": int(os.environ["N_PATCHES"]) if os.environ.get("N_PATCHES") else None,
    "precision": os.environ["PRECISION"],
    "train_stats": Path(os.environ["TRAIN_STATS_PATH"]).name,
    "eval_stats": Path(os.environ["EVAL_STATS_PATH"]).name,
    "eval_config": os.environ["EVAL_CONFIG"],
    "checkpoint": Path(os.environ["CKPT"]).name if os.environ.get("CKPT") else None,
    "data_root": os.environ["DATA_ROOT"],
    "git_commit": os.environ["GIT_COMMIT"],
    "degrade": degrade,
}
(cell / "manifest.json").write_text(json.dumps(manifest, indent=2))
print(f"wrote {cell/'metrics.json'}, confusion_matrix.csv, manifest.json")
PY
fi

echo "[done] $MODE / $CONFIG / seed$SEED"
