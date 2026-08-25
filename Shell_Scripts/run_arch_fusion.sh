#!/usr/bin/env bash
# run_arch_fusion.sh -- train ONE config under the multi-branch fusion encoder
# (--arch mbfusion), the THIRD arm of the v3 architecture comparison.
#
# Design + rationale: DL_Pipeline_v2/factorial_experiment/PLAN.md Section 6.
# Mirrors run_arch_compare.sh exactly; introduces no new patterns. Each seed
# defers to the shared idempotent run_config.sh with:
#   ARCH=mbfusion  GATE_KERNEL=3
#   CELL_NAME=<config>_mbfusion          -> distinct cell dir
#   RESULTS_DIR=Models/results_arch_fusion_v3
#     (run_config adds the /<mode>/ level -> <root>/<mode>/<config>_mbfusion/seed<k>)
#
# The branch->channel map is NOT passed here. The trainer derives it from the
# config's stats file (stats["predictor_names"], post one-hot expansion) and
# stores it in the checkpoint + .meta.json sidecar, so eval/predict auto-detect
# it and a nolidar/leafon config simply yields fewer branches.
#
# THREE ARMS, SAME SEEDS -- all three must exist at the same seeds or the paired
# per-seed comparison silently degrades to however many overlap:
#   unet       Models/factorial_results_v3/<mode>/<config>/seed<k>          (base grid)
#   unet3plus  Models/results_arch_v3/<mode>/<config>_unet3plus/seed<k>     (run_arch_compare.sh)
#   mbfusion   Models/results_arch_fusion_v3/<mode>/<config>_mbfusion/seed<k>  (this script)
#
# All three default to SEEDS="0 1 2 3 4", so a full v3 grid plus this script and
# run_arch_compare.sh already line up -- no top-up run is needed.
#
# Memory: params are ~1.3x the U-Net, but the binding constraint is ACTIVATIONS --
# at level 0 the fused tensor is 144 channels at 256^2 vs the U-Net's 64 (2.25x).
# Defaults to BATCH_SIZE=8; halve to 4 on CUDA OOM. Lighter than UNet3+.
#
# Watch for gate collapse: TensorBoard scalars train/gate_entropy/level0..5.
# Healthy is near log(n_branch) (1.386 for 4 branches); a run trending toward 0
# in the first few epochs has collapsed onto one branch.
#
# Usage:    run_arch_fusion.sh <config>
# Example:  run_arch_fusion.sh fld_chmret_leafoff
# Knobs:    MODE=multiclass|binary  SEEDS="0 1 2 3 4"  GATE_KERNEL=3
#           BATCH_SIZE=8  plus all run_config.sh env.  DRY_RUN=1 to print the plan.
#
# Long job: launch inside screen/tmux on the GPU node.
set -uo pipefail

CONFIG="${1:?usage: run_arch_fusion.sh <config>}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SEEDS="${SEEDS:-0 1 2 3 4}"
MODE="${MODE:-multiclass}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/Models/results_arch_fusion_v3}"
UNET_DIR="$REPO_ROOT/Models/factorial_results_v3/$MODE/$CONFIG"
U3P_DIR="$REPO_ROOT/Models/results_arch_v3/$MODE/${CONFIG}_unet3plus"

# mbfusion knobs (fair comparison: bf/depth inherited from run_config.sh defaults).
export ARCH="mbfusion"
export GATE_KERNEL="${GATE_KERNEL:-3}"
export BATCH_SIZE="${BATCH_SIZE:-8}"
export CELL_NAME="${CONFIG}_mbfusion"
export RESULTS_DIR MODE

read -ra SEED_ARR <<< "$SEEDS"
total=${#SEED_ARR[@]}

echo "Fusion arch arm: config=$CONFIG  mode=$MODE  arch=$ARCH (gate_kernel=$GATE_KERNEL)"
echo "seeds:     ${SEED_ARR[*]}   ($total cells)"
echo "batch:     $BATCH_SIZE"
echo "results:   $RESULTS_DIR/$MODE/$CELL_NAME"
echo "unet arm:  $UNET_DIR"
echo "u3p  arm:  $U3P_DIR"
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
echo "================ arch-fusion summary ================"
echo "cells attempted: $total"
if (( ${#failed[@]} )); then
    echo "FAILED (${#failed[@]}): ${failed[*]}"
    echo "Re-run run_arch_fusion.sh $CONFIG to retry only the unfinished cells."
    exit 1
fi

# Report seed coverage per arm -- the paired comparison uses the INTERSECTION,
# so a missing baseline seed quietly shrinks n rather than erroring.
echo "all cells complete."
echo
echo "seed coverage (paired comparison uses the intersection):"
for label in "unet:$UNET_DIR" "unet3plus:$U3P_DIR" "mbfusion:$RESULTS_DIR/$MODE/$CELL_NAME"; do
    name="${label%%:*}"; dir="${label#*:}"
    found=""
    for s in "${SEED_ARR[@]}"; do
        [[ -f "$dir/seed$s/metrics.json" ]] && found+="$s "
    done
    printf "  %-10s %s\n" "$name" "${found:-<none>}"
done
echo
echo "Then aggregate the three arms on the CPU node after sync-back:"
echo "  python $REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/dl_08b_aggregate_patchcurve.py \\"
echo "      --arch-compare --config $CONFIG --mode $MODE \\"
echo "      --arch-dir unet=$REPO_ROOT/Models/factorial_results_v3/$MODE \\"
echo "      --arch-dir unet3plus=$REPO_ROOT/Models/results_arch_v3/$MODE \\"
echo "      --arch-dir mbfusion=$RESULTS_DIR/$MODE"
