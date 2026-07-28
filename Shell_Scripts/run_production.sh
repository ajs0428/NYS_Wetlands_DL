#!/usr/bin/env bash
# run_production.sh -- train the single deployable production model (all seeds).
#
# Deliberately a THIN wrapper over run_config.sh, not a second training path.
# The production model has to be trained the same way as the factorial cells
# that justified its recipe, so this script sets a results root + a cell name +
# the recipe's knobs, and then hands off. Any fix to the training/extraction
# logic lands in run_config.sh once and both paths get it.
#
# Recipe (config, mode, arch, schedule, seeds) is NOT defined here -- it comes
# from Python_Code_Analysis/DL_Pipeline_v2/production_model/dl_prod_config.py.
#
# Idempotent, via run_config.sh's skip-completed guard: a seed whose
# metrics.json + manifest.json exist is skipped, so this survives stop/resume
# across BioHPC reservation windows. Long job: run inside screen/tmux.
#
# Usage:   run_production.sh [seed ...]        (default: the recipe's seeds)
# Example: DRY_RUN=1 run_production.sh
#          run_production.sh 0                 (just seed 0)
#
# Knobs (env overrides, all optional -- defaults come from the recipe):
#   MODE EPOCHS BATCH_SIZE BASE_FILTERS DEPTH PRECISION ARCH LEAKAGE_GUARD
#   RESULTS_DIR STATS_DIR DATA_ROOT PYTHON DRY_RUN
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PYTHON="${PYTHON:-python}"
PROD_CFG="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2/production_model/dl_prod_config.py"

# --- Pull the recipe (single source of truth) ---
# Capture BEFORE eval: `eval "$(cmd)"` reports eval's status, not cmd's, so a
# failed emit would silently define nothing and surface as an unbound variable.
if ! RECIPE_VARS="$("$PYTHON" "$PROD_CFG" --emit 2>&1)"; then
    echo "[error] could not read recipe from $PROD_CFG:" >&2
    echo "$RECIPE_VARS" >&2
    echo "  hint: activate the env first (conda activate wetland-cnn), or set PYTHON=" >&2
    exit 1
fi
eval "$RECIPE_VARS"

# Env overrides win over the recipe; the recipe wins over run_config.sh defaults.
MODE="${MODE:-$PROD_MODE}"
ARCH="${ARCH:-$PROD_ARCH}"
EPOCHS="${EPOCHS:-$PROD_EPOCHS}"
BATCH_SIZE="${BATCH_SIZE:-$PROD_BATCH_SIZE}"
BASE_FILTERS="${BASE_FILTERS:-$PROD_BASE_FILTERS}"
DEPTH="${DEPTH:-$PROD_DEPTH}"
PRECISION="${PRECISION:-$PROD_PRECISION}"
LEAKAGE_GUARD="${LEAKAGE_GUARD:-$PROD_LEAKAGE_GUARD}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/$PROD_RESULTS_SUBDIR}"

# Seeds: positional args override the recipe's list.
if [[ $# -gt 0 ]]; then SEEDS="$*"; else SEEDS="$PROD_SEEDS"; fi
read -ra SEED_ARR <<< "${SEEDS//$'\n'/ }"

echo "=============================================================="
echo " PRODUCTION MODEL -- ${#SEED_ARR[@]} seed(s)"
echo " config:  $PROD_CONFIG   mode: $MODE   arch: $ARCH bf$BASE_FILTERS d$DEPTH"
echo " sched:   $EPOCHS epochs, batch $BATCH_SIZE, $PRECISION, guard $LEAKAGE_GUARD"
echo " seeds:   ${SEED_ARR[*]}"
echo " results: $RESULTS_DIR/$MODE/$PROD_CELL_NAME/seed<k>/"
echo "=============================================================="
echo

rc=0
for seed in "${SEED_ARR[@]}"; do
    MODE="$MODE" ARCH="$ARCH" EPOCHS="$EPOCHS" BATCH_SIZE="$BATCH_SIZE" \
    BASE_FILTERS="$BASE_FILTERS" DEPTH="$DEPTH" PRECISION="$PRECISION" \
    LEAKAGE_GUARD="$LEAKAGE_GUARD" RESULTS_DIR="$RESULTS_DIR" \
    CELL_NAME="$PROD_CELL_NAME" \
        bash "$SCRIPT_DIR/run_config.sh" "$PROD_CONFIG" "$seed" || {
            rc=$?; echo "[warn] seed $seed exited $rc -- continuing"; }
done

echo
if [[ $rc -eq 0 ]]; then echo "[done] production model: all seeds complete."
else echo "[done] production model: finished with at least one failing seed (last rc=$rc)."; fi
exit $rc
