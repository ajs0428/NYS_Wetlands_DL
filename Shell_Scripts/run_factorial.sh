#!/usr/bin/env bash
# run_factorial.sh -- drive the whole factorial-v3 grid: every (mode x seed x config).
#
# Phase 2.2 driver. Walks mode outer, seed middle, config inner: within a mode,
# after seed 0 every config has one replicate -> a full single-mode factorial
# early (Decision 4.3). Calls run_config.sh, whose skip-completed guard no-ops
# finished cells, so it is safe to stop (end of a BioHPC reservation) and rerun.
#
# v3 defaults to BOTH classification modes at R=5 replicates -- 8 configs x
# 2 modes x 5 seeds = 80 cells. R=5 (v2 used 3) so the three-arm architecture
# comparison and the factorial share one seed set: no per-config top-up run,
# and every paired contrast is computed on the same five splits.
# Set MODES="multiclass" to run one mode, SEEDS="0 1 2" to fall back to R=3.
#
# Long job: launch inside screen/tmux on the GPU node so an SSH disconnect does
# not kill it.
#
# Usage:  run_factorial.sh
# Knobs:  MODES="multiclass binary"  SEEDS="0 1 2 3 4"  CONFIGS="<subset>"
#         LEAKAGE_GUARD=huc12  plus all run_config.sh knobs.  DRY_RUN=1 to plan.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PYTHON="${PYTHON:-python}"

MODES="${MODES:-multiclass binary}"
SEEDS="${SEEDS:-0 1 2 3 4}"
CONFIGS="${CONFIGS:-$("$PYTHON" "$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/dl_experiment_config.py" --list)}"

# Normalize newline/space-separated lists to arrays.
read -ra MODE_ARR   <<< "${MODES//$'\n'/ }"
read -ra SEED_ARR   <<< "${SEEDS//$'\n'/ }"
read -ra CONFIG_ARR <<< "${CONFIGS//$'\n'/ }"
total=$(( ${#MODE_ARR[@]} * ${#SEED_ARR[@]} * ${#CONFIG_ARR[@]} ))

echo "Factorial v3: ${#MODE_ARR[@]} modes x ${#SEED_ARR[@]} seeds x ${#CONFIG_ARR[@]} configs = $total cells"
echo "modes:   ${MODE_ARR[*]}"
echo "seeds:   ${SEED_ARR[*]}"
echo "configs: ${CONFIG_ARR[*]}"
echo "guard:   ${LEAKAGE_GUARD:-huc12}"
echo

done_n=0; failed=()
for mode in "${MODE_ARR[@]}"; do
    for seed in "${SEED_ARR[@]}"; do
        for config in "${CONFIG_ARR[@]}"; do
            done_n=$((done_n + 1))
            echo "#### [$done_n/$total] $mode / $config / seed$seed ####"
            if MODE="$mode" "$SCRIPT_DIR/run_config.sh" "$config" "$seed"; then
                :
            else
                echo "[FAIL] $mode / $config / seed$seed (continuing)"
                failed+=("$mode/$config/seed$seed")
            fi
        done
    done
done

echo
echo "================ factorial v3 summary ================"
echo "cells attempted: $total"
if (( ${#failed[@]} )); then
    echo "FAILED (${#failed[@]}): ${failed[*]}"
    echo "Re-run run_factorial.sh to retry only the unfinished cells."
    exit 1
fi
echo "all cells complete."
