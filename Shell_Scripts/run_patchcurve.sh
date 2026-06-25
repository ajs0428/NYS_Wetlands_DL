#!/usr/bin/env bash
# run_patchcurve.sh -- learning curve over training-set size for ONE config.
#
# Follow-on study (plan Phase 4): take the best factorial config and re-train it
# at increasing patch budgets to see how accuracy scales with data quantity and
# where the curve plateaus. Holds everything else constant (same config bands,
# label source, loss, architecture, bf/depth, seeds) -- only the patch cap moves.
#
# Each (level x seed) cell defers to the shared idempotent run_config.sh with:
#   N_PATCHES=<level>            -> dl_04 --n-patches caps the seed-shuffled pool
#   CELL_NAME=<config>_n<level>  -> distinct cell dir (won't collide with results/)
#   RESULTS_DIR=results_patchcurve
# The "full" level passes no cap, so it tracks the dataset as it grows (re-run
# later after adding patches to extend the curve's right end).
#
# Idempotent: a completed cell (metrics.json + manifest.json) is skipped, so the
# study survives stop/resume. Realized train size is recorded per cell in
# training_log.json (data_split) -- that, not the requested cap, is the curve's
# x-axis (aggregated by dl_08b_aggregate_patchcurve.py).
#
# Usage:    run_patchcurve.sh <config>
# Example:  run_patchcurve.sh fld_chmret_leafoff
# Knobs:    SEEDS="0 1 2"   LEVELS="100 200 300 400 500 full"   plus run_config.sh env
#           DRY_RUN=1 to print the plan without training.
#
# Long job: launch inside screen/tmux on the GPU node (see run_factorial.sh).
set -uo pipefail

CONFIG="${1:?usage: run_patchcurve.sh <config>}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SEEDS="${SEEDS:-0 1 2}"
# Patch budgets to sweep. "full" = no cap (whole dataset). Edit this one line to
# add levels (e.g. a new "600") as more patches become available.
LEVELS="${LEVELS:-100 200 300 400 500 full}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/results_patchcurve}"
export RESULTS_DIR

read -ra SEED_ARR  <<< "$SEEDS"
read -ra LEVEL_ARR <<< "$LEVELS"
total=$(( ${#SEED_ARR[@]} * ${#LEVEL_ARR[@]} ))

echo "Patch-count curve: config=$CONFIG"
echo "levels:  ${LEVEL_ARR[*]}"
echo "seeds:   ${SEED_ARR[*]}   ($total cells)"
echo "results: $RESULTS_DIR"
echo

done_n=0; failed=()
for level in "${LEVEL_ARR[@]}"; do
    if [[ "$level" == "full" ]]; then
        export CELL_NAME="${CONFIG}_nfull"; unset N_PATCHES || true
    else
        export CELL_NAME="${CONFIG}_n${level}"; export N_PATCHES="$level"
    fi
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
done

echo
echo "================ patch-curve summary ================"
echo "cells attempted: $total"
if (( ${#failed[@]} )); then
    echo "FAILED (${#failed[@]}): ${failed[*]}"
    echo "Re-run run_patchcurve.sh $CONFIG to retry only the unfinished cells."
    exit 1
fi
echo "all cells complete. Aggregate with:"
echo "  python $REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/dl_08b_aggregate_patchcurve.py --results-dir $RESULTS_DIR"
