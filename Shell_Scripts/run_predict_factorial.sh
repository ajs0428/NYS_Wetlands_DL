#!/usr/bin/env bash
# run_predict_factorial.sh -- mapped predictions for a HUC from a trained
# factorial model (class raster + per-class softmax probabilities).
#
# Follow-on study (plan Phase 6). Picks the best checkpoint for a config and runs
# the existing in-memory inference path (dl_06b_predict_huc.py + dl_huc_stack.py),
# which assembles the predictor stack from the canonical per-HUC source rasters --
# no *_stack.tif on disk. The config's TRAIN_STATS file IS the band/channel
# contract, so the right predictors are selected by name automatically.
#
# Prereqs:
#   1. Source rasters for the HUC are on this node. Pull them with:
#        SERVER=... REMOTE_ROOT=... LOCAL_ROOT=<root> \
#          Shell_Scripts/rsync_huc_sources.sh <cluster> <huc>
#   2. Verify the contract before predicting (no model needed):
#        python dl_huc_stack.py --huc <huc> --cluster <cluster> \
#          --data-root <root> --inspect
#
# Checkpoint selection: with [seed] given, uses results/<config>/seed<seed>;
# otherwise picks the seed with the highest macro_f1 from metrics.json.
#
# Usage:    run_predict_factorial.sh <config> <cluster> <huc> [seed]
# Example:  DATA_ROOT=/scratch/NYS_Wetlands_Data \
#             run_predict_factorial.sh fld_chmret_leafoff 208 041402011002
# Knobs (env): RESULTS_DIR (default Models/factorial_results, then results/)
#              DATA_ROOT / NYS_WETLANDS_DATA_ROOT  OUT_DIR  PATCH_SIZE  OVERLAP
#              PYTHON  DRY_RUN
set -euo pipefail

CONFIG="${1:?usage: run_predict_factorial.sh <config> <cluster> <huc> [seed]}"
CLUSTER="${2:?usage: run_predict_factorial.sh <config> <cluster> <huc> [seed]}"
HUC="${3:?usage: run_predict_factorial.sh <config> <cluster> <huc> [seed]}"
SEED="${4:-}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"
STATS_DIR="$REPO_ROOT/Data/Training_Data/stats"
PYTHON="${PYTHON:-python}"
OUT_DIR="${OUT_DIR:-$REPO_ROOT/Data/HUC_DL_Predictions}"
PATCH_SIZE="${PATCH_SIZE:-128}"
OVERLAP="${OVERLAP:-64}"

# Where the trained cells live (synced results first, then the live run tree).
if [[ -n "${RESULTS_DIR:-}" ]]; then
    : # honor explicit override
elif [[ -d "$REPO_ROOT/Models/factorial_results/$CONFIG" ]]; then
    RESULTS_DIR="$REPO_ROOT/Models/factorial_results"
else
    RESULTS_DIR="$REPO_ROOT/results"
fi

DATA_ROOT="${DATA_ROOT:-${NYS_WETLANDS_DATA_ROOT:-}}"
[[ -n "$DATA_ROOT" ]] || { echo "[error] set DATA_ROOT (or NYS_WETLANDS_DATA_ROOT) to the synced source tree"; exit 1; }

# --- Resolve the config's TRAIN_STATS (predictor/channel contract). ---
eval "$("$PYTHON" "$PIPE/dl_experiment_config.py" --emit "$CONFIG")"
STATS_PATH="$STATS_DIR/$TRAIN_STATS"
[[ -f "$STATS_PATH" ]] || { echo "[error] missing stats: $STATS_PATH"; exit 1; }

# --- Pick the cell (best macro_f1 seed unless one was given). ---
if [[ -z "$SEED" ]]; then
    SEED="$("$PYTHON" - "$RESULTS_DIR/$CONFIG" <<'PY'
import json, sys
from pathlib import Path
root = Path(sys.argv[1])
best_seed, best_f1 = None, -1.0
for cell in sorted(root.glob("seed*")):
    m = cell / "metrics.json"
    if not m.exists():
        continue
    f1 = json.loads(m.read_text()).get("macro_f1", -1.0)
    if f1 > best_f1:
        best_f1, best_seed = f1, cell.name.replace("seed", "")
if best_seed is None:
    sys.exit("no metrics.json under " + str(root))
print(best_seed)
PY
)"
    echo "selected best seed by macro_f1: seed$SEED"
fi
CELL="$RESULTS_DIR/$CONFIG/seed$SEED"
[[ -d "$CELL" ]] || { echo "[error] no cell: $CELL"; exit 1; }

# --- Locate the best checkpoint (prefer self-describing safetensors). ---
CKPT="$(ls -t "$CELL"/best_*.safetensors 2>/dev/null | head -1 || true)"
[[ -z "$CKPT" ]] && CKPT="$(ls -t "$CELL"/best_*.ckpt 2>/dev/null | head -1 || true)"
[[ -n "$CKPT" ]] || { echo "[error] no best_* checkpoint in $CELL"; exit 1; }

# --- Model shape from the cell manifest (auto-detected from ckpt too, but be explicit). ---
read -r BF DEPTH MARCH < <("$PYTHON" - "$CELL/manifest.json" <<'PY'
import json, sys
m = json.loads(open(sys.argv[1]).read())
print(m.get("base_filters", 64), m.get("depth", 5), m.get("arch", "unet"))
PY
)

echo "=============================================================="
echo " predict:   $CONFIG  seed$SEED   (arch: $MARCH, auto-detected from checkpoint)"
echo " huc:       cluster $CLUSTER / huc $HUC"
echo " model:     $CKPT  (bf$BF d$DEPTH)"
echo " stats:     $TRAIN_STATS"
echo " data-root: $DATA_ROOT"
echo " out:       $OUT_DIR"
echo "=============================================================="

run() { echo "+ $*"; [[ "${DRY_RUN:-0}" == "1" ]] || "$@"; }

run "$PYTHON" "$PIPE/dl_06b_predict_huc.py" \
    --huc "$HUC" --cluster "$CLUSTER" \
    --data-root "$DATA_ROOT" \
    --model "$CKPT" \
    --stats "$STATS_PATH" \
    --base-filters "$BF" --depth "$DEPTH" \
    --patch-size "$PATCH_SIZE" --overlap "$OVERLAP" \
    --probs \
    --out-dir "$OUT_DIR"

echo "[done] predictions for $CONFIG seed$SEED -> $OUT_DIR (class + _probs.tif)"
