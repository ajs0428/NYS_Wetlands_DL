#!/usr/bin/env bash
# run_factorial.sh -- drive the whole factorial: every (config x seed) cell.
#
# Phase 2.2 driver. Walks a fixed order (seed outer, config inner -- per Decision
# 4.3, so after seed 0 every config has one replicate -> a full factorial cell
# early) and calls run_config.sh, whose skip-completed guard no-ops finished
# cells. Safe to stop (end of a BioHPC reservation) and rerun later; it resumes.
#
# Long job (8-40 GPU-h): launch inside screen/tmux on the GPU node so an SSH
# disconnect does not kill it (AGENTS.md Long-Running Jobs).
#
# Usage:    run_factorial.sh
# Knobs:    SEEDS="0 1 2"   CONFIGS="<subset>"   plus all run_config.sh knobs
#           DRY_RUN=1 to print the plan without training.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PYTHON="${PYTHON:-python}"

SEEDS="${SEEDS:-0 1 2}"
CONFIGS="${CONFIGS:-$("$PYTHON" "$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/dl_experiment_config.py" --list)}"

# --list is newline-separated; env overrides are space-separated. Normalize both
# to whitespace-split arrays (a here-string read stops at the first newline).
read -ra SEED_ARR <<< "${SEEDS//$'\n'/ }"
read -ra CONFIG_ARR <<< "${CONFIGS//$'\n'/ }"
total=$(( ${#SEED_ARR[@]} * ${#CONFIG_ARR[@]} ))

echo "Factorial: ${#CONFIG_ARR[@]} configs x ${#SEED_ARR[@]} seeds = $total cells"
echo "seeds:   ${SEED_ARR[*]}"
echo "configs: ${CONFIG_ARR[*]}"
echo

done_n=0; failed=()
for seed in "${SEED_ARR[@]}"; do
    for config in "${CONFIG_ARR[@]}"; do
        done_n=$((done_n + 1))
        echo "#### [$done_n/$total] $config / seed$seed ####"
        if "$SCRIPT_DIR/run_config.sh" "$config" "$seed"; then
            :
        else
            echo "[FAIL] $config / seed$seed (continuing)"
            failed+=("$config/seed$seed")
        fi
    done
done

echo
echo "================ factorial summary ================"
echo "cells attempted: $total"
if (( ${#failed[@]} )); then
    echo "FAILED (${#failed[@]}): ${failed[*]}"
    echo "Re-run run_factorial.sh to retry only the unfinished cells."
    exit 1
fi
echo "all cells complete."
