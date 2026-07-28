#!/usr/bin/env bash
# run_fld_nolidar_leafoff.sh -- one config across all modes x seeds (factorial-v2 Phase 2.2 wrapper).
# Thin: defers to the shared idempotent runner. Knobs: MODES, SEEDS + run_config.sh env.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODES="${MODES:-multiclass binary}"
SEEDS="${SEEDS:-0 1 2}"
for mode in $MODES; do
  for seed in $SEEDS; do
    MODE="$mode" "$SCRIPT_DIR/run_config.sh" "fld_nolidar_leafoff" "$seed"
  done
done
