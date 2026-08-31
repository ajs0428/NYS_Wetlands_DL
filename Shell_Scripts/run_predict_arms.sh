#!/usr/bin/env bash
# run_predict_arms.sh -- batch HUC inference for ONE architecture arm in ONE mode.
#
# A host-side wrapper around run_predict_factorial.sh: builds the docker1
# invocation (two mounts + the arm's env knobs) and loops over Shell_Scripts/huc.txt.
# Written because the equivalent one-liner is ~20 lines of shell that has to be
# pasted correctly into a terminal; this is the same thing, checked in.
#
# The arm knobs it sets are the ones documented in EXECUTION.md Section 10:
#   unet      -> base grid, Models/factorial_results_v3/<mode>
#   unet3plus -> CELL_NAME=<config>_unet3plus, Models/results_arch_v3/<mode>
#   mbfusion  -> CELL_NAME=<config>_mbfusion, Models/results_arch_fusion_v3/<mode>
# The NETWORK is auto-detected from the checkpoint's .meta.json sidecar; only the
# PATH needs pointing. Output dirs are arm-specific (the filename has no arch
# token), but multiclass/binary safely share one dir -- dl_06b names by mode.
#
# Idempotent: a HUC whose *_probs.tif already exists is skipped, so an interrupted
# run resumes where it stopped. Deliberately not parallel -- both arms want the GPU.
#
# Usage:    Shell_Scripts/run_predict_arms.sh <unet|unet3plus|mbfusion> [multiclass|binary] [seed]
# Examples: Shell_Scripts/run_predict_arms.sh unet     binary
#           Shell_Scripts/run_predict_arms.sh mbfusion binary
#           DRY_RUN=1 Shell_Scripts/run_predict_arms.sh mbfusion binary   # resolve only
# Knobs:    SEED (default 2)  CONFIG (default fld_chmret_leafoff)
#           HUC_LIST (default Shell_Scripts/huc.txt)  DATA_DIR  DRY_RUN
#
# Long job (~3 h per arm for 30 HUCs): launch inside tmux.
set -uo pipefail

ARM="${1:?usage: run_predict_arms.sh <unet|unet3plus|mbfusion> [multiclass|binary] [seed]}"
MODE="${2:-multiclass}"
SEED="${3:-${SEED:-2}}"

CONFIG="${CONFIG:-fld_chmret_leafoff}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DATA_DIR="${DATA_DIR:-/workdir/$USER/NYS_Wetlands_Data}"
HUC_LIST="${HUC_LIST:-$SCRIPT_DIR/huc.txt}"
IMAGE="${IMAGE:-nys-wetlands-dl}"

case "$MODE" in multiclass|binary) ;; *) echo "[error] mode must be multiclass|binary, got '$MODE'"; exit 1 ;; esac

# --- Per-arm knobs. Container paths (/app/...) -- these are read INSIDE. -------
case "$ARM" in
  unet)
    CELL_NAME="$CONFIG"
    RESULTS_DIR="/app/Models/factorial_results_v3/$MODE"
    OUT_DIR="/app/Data/HUC_DL_Predictions_v3"
    ;;
  mbfusion)
    CELL_NAME="${CONFIG}_mbfusion"
    RESULTS_DIR="/app/Models/results_arch_fusion_v3/$MODE"
    OUT_DIR="/app/Data/HUC_DL_Predictions_v3_mbfusion"
    ;;
  unet3plus)
    CELL_NAME="${CONFIG}_unet3plus"
    RESULTS_DIR="/app/Models/results_arch_v3/$MODE"
    OUT_DIR="/app/Data/HUC_DL_Predictions_v3_unet3plus"
    ;;
  *) echo "[error] unknown arm '$ARM' (expected unet | mbfusion | unet3plus)"; exit 1 ;;
esac
HOST_OUT="$REPO_ROOT/${OUT_DIR#/app/}"
HOST_CELL="$REPO_ROOT/${RESULTS_DIR#/app/}/$CELL_NAME/seed$SEED"

# --- Host-side preflight: fail here, not 3 hours in. --------------------------
fail=0
[[ -d "$REPO_ROOT/$(basename "$DATA_DIR")" || -d "$DATA_DIR" ]] || { echo "[error] no source tree at $DATA_DIR"; fail=1; }
[[ -f "$HUC_LIST" ]] || { echo "[error] no HUC list: $HUC_LIST"; fail=1; }
[[ -d "$HOST_CELL" ]] || { echo "[error] no cell: $HOST_CELL"; fail=1; }
ls "$HOST_CELL"/best_*.safetensors >/dev/null 2>&1 \
  || ls "$HOST_CELL"/best_*.ckpt   >/dev/null 2>&1 \
  || { echo "[error] no best_* checkpoint in $HOST_CELL"; fail=1; }
docker1 images --format '{{.Repository}}' 2>/dev/null | grep -qx "$IMAGE" \
  || { echo "[error] image '$IMAGE' not loaded (docker1 load -i ...tar.gz)"; fail=1; }
(( fail )) && exit 1

n_huc=$(grep -cE '^[^[:space:]#]+:[^[:space:]]+' "$HUC_LIST")
echo "=============================================================="
echo " arm:      $ARM      mode: $MODE      seed: $SEED"
echo " config:   $CONFIG"
echo " cell:     $CELL_NAME   ($HOST_CELL)"
echo " hucs:     $n_huc  (from $HUC_LIST)"
echo " out:      $HOST_OUT"
echo " already:  $(ls "$HOST_OUT"/DLpred_${MODE}_*_probs.tif 2>/dev/null | wc -l) of $n_huc done -- these will be skipped"
echo " free:     $(df -h "$REPO_ROOT" | tail -1 | awk '{print $4}')"
echo "=============================================================="
if [[ "${DRY_RUN:-0}" == "1" ]]; then
    echo "[dry-run] resolving one HUC through run_predict_factorial.sh, then stopping:"
    read -r c h < <(grep -m1 -E '^[^[:space:]#]+:[^[:space:]]+' "$HUC_LIST" | tr ':' ' ')
    exec docker1 run --rm --user "$(id -u):$(id -g)" \
      -v "/workdir/$USER/nys_wetlands:/app" -v "$DATA_DIR:/data" \
      -e DATA_ROOT=/data -e TMPDIR=/app/tmp -e DRY_RUN=1 \
      -e MODE="$MODE" -e CELL_NAME="$CELL_NAME" -e RESULTS_DIR="$RESULTS_DIR" -e OUT_DIR="$OUT_DIR" \
      "$IMAGE" bash Shell_Scripts/run_predict_factorial.sh "$CONFIG" "$c" "$h" "$SEED"
fi

start=$(date +%s)
docker1 run --rm --gpus all --shm-size=8g --user "$(id -u):$(id -g)" \
  -v "/workdir/$USER/nys_wetlands:/app" -v "$DATA_DIR:/data" \
  -e DATA_ROOT=/data -e TMPDIR=/app/tmp \
  -e MODE="$MODE" -e CELL_NAME="$CELL_NAME" -e RESULTS_DIR="$RESULTS_DIR" -e OUT_DIR="$OUT_DIR" \
  -e PRED_CONFIG="$CONFIG" -e PRED_SEED="$SEED" \
  "$IMAGE" bash -c '
    while IFS=: read -r cluster huc; do
      cluster="${cluster%%[[:space:]]*}"; huc="${huc%%[[:space:]]*}"
      [[ -z "$cluster" || -z "$huc" || "$cluster" == \#* ]] && continue
      if [[ -s "$OUT_DIR/DLpred_${MODE}_cluster_${cluster}_huc_${huc}_probs.tif" ]]; then
        echo "[skip] $cluster:$huc"; continue
      fi
      echo "=== $(date +%H:%M:%S)  $MODE  $cluster:$huc ==="
      bash Shell_Scripts/run_predict_factorial.sh "$PRED_CONFIG" "$cluster" "$huc" "$PRED_SEED" \
        || echo "[FAILED] cluster $cluster huc $huc"
    done < /app/Shell_Scripts/huc.txt'
rc=$?

el=$(( $(date +%s) - start ))
done_n=$(ls "$HOST_OUT"/DLpred_${MODE}_*_probs.tif 2>/dev/null | wc -l)
echo
echo "================ $ARM / $MODE summary ================"
printf " elapsed:  %dh %dm\n" $((el/3600)) $(((el%3600)/60))
echo " complete: $done_n / $n_huc probability rasters in $HOST_OUT"
echo " size:     $(du -sh "$HOST_OUT" 2>/dev/null | cut -f1)   free: $(df -h "$REPO_ROOT" | tail -1 | awk '{print $4}')"
(( done_n < n_huc )) && echo " NOTE: incomplete -- re-run this exact command to resume (finished HUCs are skipped)."
exit $rc
