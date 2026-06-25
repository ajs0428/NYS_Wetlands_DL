#!/usr/bin/env bash
# run_arch_compare.sh -- re-train ONE config under UNet3+ to gauge how much the
# architecture (vs U-Net) drives accuracy.
#
# Follow-on study (plan Phase 5). The U-Net baseline for this config already
# lives in results/<config>/ (the base factorial, R=3), so this only adds the
# UNet3+ arm on the SAME seeds and the SAME seed-determined test patches -> a
# paired, apples-to-apples comparison.
#
# Fair comparison: capacity is held at the baseline's bf=64/d5; only the
# architecture changes. Deep supervision is ON by default (treated as part of
# the UNet3+ definition for this study, not a tested axis).
#
# Each seed defers to the shared idempotent run_config.sh with:
#   ARCH=unet3plus  DEEP_SUPERVISION=1  CAT_CHANNELS=64
#   CELL_NAME=<config>_unet3plus        -> distinct cell dir
#   RESULTS_DIR=results_arch
#
# Memory: UNet3+ at bf=64/d5 is heavy. Defaults to 16-mixed (factorial default)
# and a reduced batch size (8); drop to BATCH_SIZE=4 if you hit OOM. eval auto-
# detects arch from the checkpoint, so no extra flags are needed downstream.
#
# Usage:    run_arch_compare.sh <config>
# Example:  run_arch_compare.sh fld_chmret_leafoff
# Knobs:    SEEDS="0 1 2"  CAT_CHANNELS=64  DEEP_SUPERVISION=1  BATCH_SIZE=8
#           plus all run_config.sh env.  DRY_RUN=1 to print the plan.
#
# Long job: launch inside screen/tmux on the GPU node.
set -uo pipefail

CONFIG="${1:?usage: run_arch_compare.sh <config>}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SEEDS="${SEEDS:-0 1 2}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/results_arch}"

# UNet3+ knobs (fair comparison: bf/depth inherited from run_config.sh defaults).
export ARCH="unet3plus"
export DEEP_SUPERVISION="${DEEP_SUPERVISION:-1}"
export CAT_CHANNELS="${CAT_CHANNELS:-64}"
export BATCH_SIZE="${BATCH_SIZE:-8}"
export CELL_NAME="${CONFIG}_unet3plus"
export RESULTS_DIR

read -ra SEED_ARR <<< "$SEEDS"
total=${#SEED_ARR[@]}

echo "Architecture comparison: config=$CONFIG  arch=$ARCH (deep_supervision=$DEEP_SUPERVISION)"
echo "seeds:    ${SEED_ARR[*]}   ($total cells)"
echo "batch:    $BATCH_SIZE   cat_channels=$CAT_CHANNELS"
echo "results:  $RESULTS_DIR/$CELL_NAME"
echo "baseline: $REPO_ROOT/results/$CONFIG  (U-Net, same seeds -- already trained)"
echo

done_n=0; failed=()
for seed in "${SEED_ARR[@]}"; do
    done_n=$((done_n + 1))
    echo "#### [$done_n/$total] $CELL_NAME / seed$seed ####"
    if "$SCRIPT_DIR/run_config.sh" "$CONFIG" "$seed"; then
        :
    else
        echo "[FAIL] $CELL_NAME / seed$seed (continuing)"
        failed+=("$CELL_NAME/seed$seed")
    fi
done

echo
echo "================ arch-compare summary ================"
echo "cells attempted: $total"
if (( ${#failed[@]} )); then
    echo "FAILED (${#failed[@]}): ${failed[*]}"
    echo "Re-run run_arch_compare.sh $CONFIG to retry only the unfinished cells."
    exit 1
fi
echo "all cells complete. Compare U-Net vs UNet3+ with:"
echo "  python $REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/dl_08b_aggregate_patchcurve.py \\"
echo "      --arch-compare --config $CONFIG --unet-dir $REPO_ROOT/results --unet3plus-dir $RESULTS_DIR"
