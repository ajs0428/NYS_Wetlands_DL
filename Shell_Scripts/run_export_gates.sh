#!/usr/bin/env bash
# run_export_gates.sh -- export per-scale branch-gate rasters for the mbfusion
# cells (EXECUTION.md Section 9.2). A thin loop over dl_11_export_gates.py, so
# the container command is one line instead of a pasted multi-line heredoc.
#
# Writes <cell>/gates/<patch>.npz (six float16 (n_branch,H,W) arrays) plus
# gate_summary.json, from a deterministic prefix of the seed's held-out FIELD
# patches. rsync_results.sh --metrics-only includes *.npz, so these ride back
# with the JSON/CSV.
#
# Reading the maps: within-branch SPATIAL comparison is valid; cross-branch
# ABSOLUTE comparison is confounded (post-gate proj is a 1x1 conv). Take overall
# branch importance from SHAP. PLAN.md Section 6.7.
#
# Usage (on the GPU node, inside the container):
#   bash Shell_Scripts/run_export_gates.sh
#
# Knobs: MODES="multiclass binary"  SEEDS="0 1 2 3 4"  CONFIG=fld_chmret_leafoff
#        N_PATCHES=8  RESULTS_DIR=Models/results_arch_fusion_v3  DRY_RUN=1
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIPE="$REPO_ROOT/Python_Code_Analysis/DL_Pipeline_v2"

MODES="${MODES:-multiclass binary}"
SEEDS="${SEEDS:-0 1 2 3 4}"
CONFIG="${CONFIG:-fld_chmret_leafoff}"
N_PATCHES="${N_PATCHES:-8}"
RESULTS_DIR="${RESULTS_DIR:-$REPO_ROOT/Models/results_arch_fusion_v3}"
CELL_NAME="${CELL_NAME:-${CONFIG}_mbfusion}"

echo "=== export gates | config=$CONFIG modes=[$MODES] seeds=[$SEEDS] n_patches=$N_PATCHES"
echo "=== root=$RESULTS_DIR cell=$CELL_NAME"

fail=0
done_n=0
for MODE in $MODES; do
  for SEED in $SEEDS; do
    CELL="$RESULTS_DIR/$MODE/$CELL_NAME/seed$SEED"
    if [[ ! -d "$CELL" ]]; then
      echo "[SKIP] no cell: $CELL"
      continue
    fi
    if [[ -f "$CELL/gates/gate_summary.json" ]]; then
      echo "[SKIP] gates already exported: $MODE seed$SEED"
      continue
    fi
    echo "--- $MODE seed$SEED"
    if [[ "${DRY_RUN:-0}" == "1" ]]; then
      echo "      would run: dl_11_export_gates.py --cell $CELL --config $CONFIG --seed $SEED --mode $MODE"
      continue
    fi
    if python "$PIPE/dl_11_export_gates.py" \
         --cell "$CELL" --config "$CONFIG" --seed "$SEED" --mode "$MODE" \
         --n-patches "$N_PATCHES"; then
      done_n=$((done_n + 1))
    else
      echo "[FAILED] $MODE seed$SEED"
      fail=$((fail + 1))
    fi
  done
done

echo "=== exported $done_n cell(s), $fail failure(s)"
exit $(( fail > 0 ? 1 : 0 ))
