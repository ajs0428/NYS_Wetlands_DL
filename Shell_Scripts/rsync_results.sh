#!/bin/bash -l
set -euo pipefail

# rsync_results.sh
#
# Sync factorial-experiment outputs from the GPU node's local /workdir back to
# the CPU node (or any analysis host) for aggregation, plotting, and archival.
#
# BioHPC /workdir is LOCAL to each node, so results written on cbsugpu09/10
# during a reservation do not appear on the CPU login/compute node by
# themselves -- they have to be pulled across. This script does that pull,
# mirroring the run layout produced by the orchestration scripts:
#
#   <results-root>/
#     <config>/seed<k>/{manifest.json,metrics.json,confusion_matrix.csv,
#                       metrics_forest.json,best.ckpt,*.safetensors,*.meta.json,
#                       train_log.csv, ... shap outputs ...}
#     stats/multiclass_normalization_stats[_<config>]_wp0.5.json
#
# By default it pulls EVERYTHING (including ~500 MB checkpoints per run). Use
# --metrics-only for a fast, lightweight pull of just the analysis artifacts
# (JSON/CSV/PNG) -- enough for Phase 3 aggregation without moving the weights.
#
# Usage:
#   SERVER="user@cbsugpu09.biohpc.cornell.edu:" \
#   REMOTE_RESULTS="/workdir/<labid>/nys_wetlands/results" \
#   LOCAL_DEST="../Models/factorial_results" \
#     ./rsync_results.sh [CONFIG ...] [--metrics-only] [-n|--dry-run] [--delete]
#
#   - CONFIG ...    : optional config names to limit the sync (default: all).
#                     e.g. fld_chmret_leafoff nwi_chmret_leafoff
#   - --metrics-only: skip checkpoints/weights; pull JSON/CSV/PNG + manifests.
#   - -n|--dry-run  : show what rsync would transfer, move nothing.
#   - --delete      : mirror deletions (prune local files gone from remote).
#
# Example (pull just the small metrics for all configs):
#   SERVER="ajs544@cbsugpu09.biohpc.cornell.edu:" \
#   REMOTE_RESULTS="/workdir/ajs544/nys_wetlands/results" \
#   LOCAL_DEST="/ibstorage/anthony/NYS_Wetlands_DL/Models/factorial_results" \
#     ./rsync_results.sh --metrics-only

# === CONFIGURATION (override via environment) ===
# SERVER: rsync/ssh host prefix WITH trailing colon (e.g. "user@host:").
#         Leave empty ("") to copy from a local REMOTE_RESULTS (handy for testing).
SERVER="${SERVER:-}"
# REMOTE_RESULTS: path to the results/ dir on the GPU node (no trailing slash).
REMOTE_RESULTS="${REMOTE_RESULTS:?Set REMOTE_RESULTS to the results dir on the GPU node}"
# LOCAL_DEST: destination results dir on the analysis host.
LOCAL_DEST="${LOCAL_DEST:-$(pwd)/factorial_results}"
# Extra rsync flags (e.g. RSYNC_OPTS="-avz --progress -e 'ssh -p 2222'").
RSYNC_OPTS="${RSYNC_OPTS:--avz --progress}"

# === ARGS ===
DRY_RUN=0
METRICS_ONLY=0
DELETE=0
CONFIGS=()
for a in "$@"; do
    case "$a" in
        -n|--dry-run)    DRY_RUN=1 ;;
        --metrics-only)  METRICS_ONLY=1 ;;
        --delete)        DELETE=1 ;;
        -h|--help)
            grep -E '^#( |$)' "$0" | sed -E 's/^# ?//'
            exit 0 ;;
        -*)
            echo "Unknown flag: $a" >&2; exit 1 ;;
        *)               CONFIGS+=("$a") ;;
    esac
done

# ssh host = SERVER without the trailing colon; empty means local copy.
SSH_HOST="${SERVER%:}"

# Assemble rsync option list.
OPTS=($RSYNC_OPTS)
[ "$DRY_RUN" -eq 1 ] && OPTS+=(--dry-run)
[ "$DELETE"  -eq 1 ] && OPTS+=(--delete)
if [ "$METRICS_ONLY" -eq 1 ]; then
    # Keep only lightweight analysis artifacts; drop weights/checkpoints.
    OPTS+=(--include='*/')
    OPTS+=(--include='*.json' --include='*.csv' --include='*.png')
    OPTS+=(--exclude='*')
fi

echo "=============================================================="
echo " Syncing factorial results back for analysis"
echo "   from:         ${SERVER:-<local>}${REMOTE_RESULTS}"
echo "   to:           $LOCAL_DEST"
if [ "${#CONFIGS[@]}" -gt 0 ]; then
    echo "   configs:      ${CONFIGS[*]}"
else
    echo "   configs:      <all>"
fi
[ "$METRICS_ONLY" -eq 1 ] && echo "   mode:         METRICS ONLY (no checkpoints)"
[ "$DELETE"       -eq 1 ] && echo "   delete:       ON (mirror remote deletions)"
[ "$DRY_RUN"      -eq 1 ] && echo "   run:          DRY RUN (no files moved)"
echo "=============================================================="

mkdir -p "$LOCAL_DEST"

transfer() {
    local rel="$1"          # path under results/ (e.g. "fld_chm_leafon" or "")
    local src="${SERVER}${REMOTE_RESULTS}/${rel}"
    local dest="$LOCAL_DEST/${rel}"
    mkdir -p "$dest"
    # Trailing slashes: copy CONTENTS of src into dest.
    # shellcheck disable=SC2086
    rsync "${OPTS[@]}" "${src%/}/" "${dest%/}/"
}

if [ "${#CONFIGS[@]}" -gt 0 ]; then
    # Per-config pull; always include the shared stats/ dir too.
    for cfg in "${CONFIGS[@]}"; do
        echo "--- $cfg ---"
        transfer "$cfg"
    done
    echo "--- stats ---"
    transfer "stats"
else
    # Whole results tree in one pass.
    transfer ""
fi

echo "=============================================================="
if [ "$DRY_RUN" -eq 1 ]; then
    echo "Dry run -- nothing transferred."
else
    echo "Done. Results synced to $LOCAL_DEST"
    echo "Aggregate with the Phase 3 script (CPU node):"
    echo "  walk $LOCAL_DEST/<config>/seed*/metrics.json -> factorial table"
fi
echo "=============================================================="
