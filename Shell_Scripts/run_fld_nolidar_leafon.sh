#!/usr/bin/env bash
# run_fld_nolidar_leafon.sh -- one config across all replicate seeds (Phase 2.2 wrapper).
# Thin: defers to the shared idempotent runner. Knobs: SEEDS + run_config.sh env.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SEEDS="${SEEDS:-0 1 2}"
for seed in $SEEDS; do
    "$SCRIPT_DIR/run_config.sh" "fld_nolidar_leafon" "$seed"
done
