#!/usr/bin/env bash
# run_aggregate.sh -- Phase 3 aggregation for the wetland factorial experiment.
#
# Thin wrapper over dl_08_aggregate_factorial.py. Pure pandas, CPU-only, no model
# loading -- so this runs on the CPU/login node after sync-back (or anywhere the
# results tree lives). It is SAFE to run on a PARTIAL tree while training is still
# going: it reports coverage (cells found of the 8 config x N seed grid) and
# computes every contrast it has the cells for, so re-running as the factorial
# fills in just extends the table.
#
# Walks  <RESULTS_DIR>/<config>/seed<k>/metrics.json  into  <RESULTS_DIR>/analysis/:
#   factorial_long.csv  factorial_summary.csv  factorial_table.csv
#   contrasts.csv  coverage.csv  confusion_mean/<config>.csv
# Contrasts are paired by seed (same seed => same split across configs).
#
# Usage:   run_aggregate.sh
# Knobs (env overrides):
#   RESULTS_DIR  results root to walk  (default: Models/factorial_results if it
#                exists -- the synced location -- else <repo>/results)
#   OUTPUT_DIR   where CSVs land       (default: <RESULTS_DIR>/analysis)
#   SEEDS        expected seeds, for the coverage report (default: "0 1 2 3 4")
#   PYTHON       interpreter           (default: python; needs the project env active)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"
PYTHON="${PYTHON:-python}"

# Default to the synced CPU-node location if present, else the GPU-node results/.
if [[ -z "${RESULTS_DIR:-}" ]]; then
    if [[ -d "$REPO_ROOT/Models/factorial_results" ]]; then
        RESULTS_DIR="$REPO_ROOT/Models/factorial_results"
    else
        RESULTS_DIR="$REPO_ROOT/results"
    fi
fi
SEEDS="${SEEDS:-0 1 2 3 4}"

[[ -d "$RESULTS_DIR" ]] || {
    echo "[error] results dir not found: $RESULTS_DIR"
    echo "  Set RESULTS_DIR to wherever the <config>/seed<k>/ cells live"
    echo "  (e.g. RESULTS_DIR=Models/factorial_results run_aggregate.sh)."
    exit 1
}

echo "=============================================================="
echo " aggregate: $RESULTS_DIR"
echo " output:    ${OUTPUT_DIR:-$RESULTS_DIR/analysis}"
echo " seeds:     $SEEDS"
echo "=============================================================="

ARGS=(--results-dir "$RESULTS_DIR" --seeds $SEEDS)
[[ -n "${OUTPUT_DIR:-}" ]] && ARGS+=(--output-dir "$OUTPUT_DIR")

exec "$PYTHON" "$PIPE/dl_08_aggregate_factorial.py" "${ARGS[@]}"
