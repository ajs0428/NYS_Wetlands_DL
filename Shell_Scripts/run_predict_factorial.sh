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
# Checkpoint selection: with [seed] given, uses <results>/<config>/seed<seed>;
# otherwise picks the seed with the highest macro_f1 from metrics.json (reads
# both the v2 nested test_metrics schema and the flat v1 one).
#
# v3: cells live under Models/factorial_results_v3/<MODE>/<config>/seed<k> and
# predictions land in Data/HUC_DL_Predictions_v3 as DLpred_<mode>_cluster_..._huc_....tif
# (dl_06b names by the stats' classification_mode and asserts it matches the model).
#
# Architecture arms: <config> always names the FACTORIAL config (it is the
# predictor/stats contract). The arch arms live in their own results roots under
# a suffixed CELL dir, so point the script at them with CELL_NAME + RESULTS_DIR:
#   unet       (default)  Models/factorial_results_v3/<mode>/<config>/
#   unet3plus  CELL_NAME=<config>_unet3plus RESULTS_DIR=Models/results_arch_v3/<mode>
#   mbfusion   CELL_NAME=<config>_mbfusion  RESULTS_DIR=Models/results_arch_fusion_v3/<mode>
# The network itself is auto-detected from the checkpoint's .meta.json sidecar
# (arch + mbfusion branch map), so no --arch flag is ever passed.
#
# Usage:    run_predict_factorial.sh <config> <cluster> <huc> [seed]
# Example:  DATA_ROOT=/scratch/NYS_Wetlands_Data \
#             run_predict_factorial.sh fld_chmret_leafoff 208 041402011002
#           MODE=binary run_predict_factorial.sh fld_chmret_leafoff 208 041402011002
# Knobs (env): MODE(multiclass|binary)
#              CELL_NAME (default <config>; the cell DIRECTORY name -- set this
#                for the arch arms, whose dirs carry an _unet3plus/_mbfusion suffix)
#              RESULTS_DIR (default Models/factorial_results_v3/<MODE>, falling
#                back to the v1 Models/factorial_results, then results/)
#              DATA_ROOT / NYS_WETLANDS_DATA_ROOT  OUT_DIR  PATCH_SIZE  OVERLAP
#              PYTHON  DRY_RUN
#              MASK (default 1) -- clip the outputs to the HUC12 boundary with
#                dl_07_mask_predictions.py. dl_06b predicts over the whole DEM
#                bounding box, ~54% of which falls OUTSIDE the watershed on
#                out-of-domain stack values, so an unmasked raster is unusable
#                for any area statistic. Set MASK=0 only to inspect raw output.
#              HUC_GPKG -- boundary vector (default: Data/NY_HUCS/ in the repo,
#                falling back to the sibling NYS_Wetlands_Data copy)
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
MODE="${MODE:-multiclass}"
# The cell DIRECTORY name. Equals <config> for the base U-Net grid; the arch arms
# suffix it (<config>_unet3plus / <config>_mbfusion). Kept separate from $CONFIG
# because $CONFIG must stay a valid dl_experiment_config key (the stats contract).
CELL_NAME="${CELL_NAME:-$CONFIG}"
PATCH_SIZE="${PATCH_SIZE:-128}"
OVERLAP="${OVERLAP:-64}"
MASK="${MASK:-1}"

# Where the trained cells live: newest mode-tokened tree first, then older
# fallbacks. v3 before v2 so a fresh grid is picked up without an env override.
if [[ -n "${RESULTS_DIR:-}" ]]; then
    : # honor explicit override (point it at the tree holding <config>/seed<k>)
elif [[ -d "$REPO_ROOT/Models/factorial_results_v3/$MODE/$CELL_NAME" ]]; then
    RESULTS_DIR="$REPO_ROOT/Models/factorial_results_v3/$MODE"
elif [[ -d "$REPO_ROOT/Models/factorial_results_v2/$MODE/$CELL_NAME" ]]; then
    RESULTS_DIR="$REPO_ROOT/Models/factorial_results_v2/$MODE"
elif [[ -d "$REPO_ROOT/Models/factorial_results/$CELL_NAME" ]]; then
    RESULTS_DIR="$REPO_ROOT/Models/factorial_results"
else
    RESULTS_DIR="$REPO_ROOT/results"
fi

DATA_ROOT="${DATA_ROOT:-${NYS_WETLANDS_DATA_ROOT:-}}"
[[ -n "$DATA_ROOT" ]] || { echo "[error] set DATA_ROOT (or NYS_WETLANDS_DATA_ROOT) to the synced source tree"; exit 1; }

# --- Resolve the config's TRAIN_STATS (predictor/channel contract). ---
eval "$("$PYTHON" "$PIPE/dl_experiment_config.py" --emit "$CONFIG" --mode "$MODE")"
STATS_PATH="$STATS_DIR/$TRAIN_STATS"
[[ -f "$STATS_PATH" ]] || { echo "[error] missing stats: $STATS_PATH"; exit 1; }

# --- Pick the cell (best macro_f1 seed unless one was given). ---
if [[ -z "$SEED" ]]; then
    SEED="$("$PYTHON" - "$RESULTS_DIR/$CELL_NAME" <<'PY'
import json, sys
from pathlib import Path
root = Path(sys.argv[1])
best_seed, best_f1 = None, -1.0
for cell in sorted(root.glob("seed*")):
    m = cell / "metrics.json"
    if not m.exists():
        continue
    metrics = json.loads(m.read_text())
    # v2 nests scores under test_metrics; v1 (dl_05) is flat.
    sc = metrics.get("test_metrics") or metrics
    f1 = sc.get("macro_f1")
    if f1 is not None and f1 > best_f1:
        best_f1, best_seed = f1, cell.name.replace("seed", "")
if best_seed is None:
    sys.exit("no usable metrics.json (with macro_f1) under " + str(root))
print(best_seed)
PY
)"
    echo "selected best seed by macro_f1: seed$SEED"
fi
CELL="$RESULTS_DIR/$CELL_NAME/seed$SEED"
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

# --- Output dir (resolved AFTER the arch is known). dl_06b names its output
# DLpred_<mode>_cluster_<C>_huc_<H>.tif with NO arch token, so two arms writing
# into one dir silently overwrite each other. Give each non-unet arm its own
# root by default; an explicit OUT_DIR always wins.
if [[ "$MARCH" == "unet" ]]; then
    OUT_DIR="${OUT_DIR:-$REPO_ROOT/Data/HUC_DL_Predictions_v3}"
else
    OUT_DIR="${OUT_DIR:-$REPO_ROOT/Data/HUC_DL_Predictions_v3_$MARCH}"
fi

echo "=============================================================="
echo " predict:   $CONFIG  seed$SEED  mode=$MODE   (arch: $MARCH, auto-detected from checkpoint)"
echo " cell:      $CELL_NAME"
echo " huc:       cluster $CLUSTER / huc $HUC"
echo " model:     $CKPT  (bf$BF d$DEPTH)"
echo " stats:     $TRAIN_STATS"
echo " data-root: $DATA_ROOT"
echo " out:       $OUT_DIR"
echo " mask:      $([[ "$MASK" == "1" ]] && echo "yes (clip to HUC12 boundary)" || echo "NO -- raw bbox, not usable for area stats")"
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

# --- Clip to the HUC12 boundary, in place. ---
# dl_06b writes the whole DEM bounding box; roughly half of it is outside the
# watershed, on stack values the model was never meant to see. Masking here --
# rather than as a later sweep -- means an unmasked raster never reaches an
# analysis notebook. Idempotent: a raster already tagged DL_MASKED is skipped,
# so re-running a HUC costs nothing.
if [[ "$MASK" == "1" ]]; then
    MASK_ARGS=(--in-dir "$OUT_DIR" --in-place
               --pattern "DLpred_${MODE}_cluster_${CLUSTER}_huc_${HUC}*.tif")
    [[ -n "${HUC_GPKG:-}" ]] && MASK_ARGS+=(--gpkg "$HUC_GPKG")
    run "$PYTHON" "$PIPE/dl_07_mask_predictions.py" "${MASK_ARGS[@]}"
else
    echo "[warn] MASK=0 -- output covers the full DEM bbox, ~54% of it outside"
    echo "       the HUC. Do not compute area statistics from it unmasked."
fi

echo "[done] predictions for $CONFIG seed$SEED -> $OUT_DIR (class + _probs.tif)"
