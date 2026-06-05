#!/bin/bash -l
set -euo pipefail

# rsync_huc_sources.sh
#
# Sync the predictor SOURCE rasters for ONE HUC (one cluster + huc id) from the
# server where they live to a local data-root, mirroring the directory layout
# that dl_huc_stack.py / dl_06b_predict_huc.py expect. This lets you run mapped
# predictions from in-memory stacks (no *_stack.tif) without copying hundreds of
# GB -- you pull only the handful of files for the HUC you are mapping.
#
# Source directories and the per-dataset file-selection rules mirror
# R_Code_Analysis/huc_stack.R (huc_source_dirs + huc_source_paths/match_one):
#   DEM     -> exclude whitebox ("wbt") intermediates
#   terrain -> the local slope file, excluding 10m / 1000m scales
#   others  -> any .tif matching the huc id and cluster
#
# Usage:
#   SERVER="user@hpc.example.edu:" REMOTE_ROOT="/projects/NYS_Wetlands_Data" \
#   LOCAL_ROOT="/scratch/NYS_Wetlands_Data" \
#     ./rsync_huc_sources.sh <CLUSTER> <HUC> [-n|--dry-run]
#
# Example:
#   SERVER="ajs@greene.hpc.nyu.edu:" REMOTE_ROOT="/scratch/ajs/NYS_Wetlands_Data" \
#   LOCAL_ROOT="/data/NYS_Wetlands_Data" \
#     ./rsync_huc_sources.sh 208 041402011002
#
# Then predict:
#   python dl_06b_predict_huc.py --huc 041402011002 --cluster 208 \
#       --data-root "$LOCAL_ROOT" --model Models/<model>.safetensors

# === CONFIGURATION (override via environment) ===
# SERVER: rsync/ssh host prefix WITH trailing colon (e.g. "user@host:").
#         Leave empty ("") to copy from a local REMOTE_ROOT (handy for testing).
SERVER="${SERVER:-}"
# REMOTE_ROOT: path on SERVER that contains the Data/ tree (no trailing slash).
REMOTE_ROOT="${REMOTE_ROOT:?Set REMOTE_ROOT to the data-project root on the server}"
# LOCAL_ROOT: destination data-root; pass this as --data-root to the predictor.
LOCAL_ROOT="${LOCAL_ROOT:-$(pwd)/NYS_Wetlands_Data_sources}"
# Extra rsync flags (e.g. RSYNC_OPTS="-avz --progress -e 'ssh -p 2222'").
RSYNC_OPTS="${RSYNC_OPTS:--avz --progress}"

# === ARGS ===
DRY_RUN=0
POSARGS=()
for a in "$@"; do
    case "$a" in
        -n|--dry-run) DRY_RUN=1 ;;
        -h|--help)
            grep -E '^#( |$)' "$0" | sed -E 's/^# ?//'
            exit 0 ;;
        *) POSARGS+=("$a") ;;
    esac
done

if [ "${#POSARGS[@]}" -lt 2 ]; then
    echo "Usage: $(basename "$0") <CLUSTER> <HUC> [-n|--dry-run]" >&2
    echo "  (set SERVER, REMOTE_ROOT, LOCAL_ROOT via environment)" >&2
    exit 1
fi
CLUSTER="${POSARGS[0]}"
HUC="${POSARGS[1]}"

# ssh host = SERVER without the trailing colon; empty means local listing.
SSH_HOST="${SERVER%:}"

# === SOURCE LAYOUT (mirrors huc_source_dirs() in huc_stack.R) ===
KEYS="dem terr hydro chm naip ortho lidar"

subdir_for() {
    case "$1" in
        dem)   echo "Data/TerrainProcessed/HUC_DEMs" ;;
        terr)  echo "Data/TerrainProcessed/HUC_TerrainMetrics" ;;
        hydro) echo "Data/TerrainProcessed/HUC_Hydro" ;;
        chm)   echo "Data/CHMs/HUC_CHMs" ;;
        naip)  echo "Data/NAIP/HUC_NAIP_Processed" ;;
        ortho) echo "Data/Ortho/HUC_Ortho" ;;
        lidar) echo "Data/Lidar/HUC_Lidar_Metrics" ;;
    esac
}

# Run a (simple) command on the server, or locally when SERVER is empty.
run_remote() {
    if [ -n "$SSH_HOST" ]; then
        ssh "$SSH_HOST" "$1"
    else
        sh -c "$1"
    fi
}

# Keep a filename for this dataset? Mirrors match_one() in huc_stack.R.
keep_file() {
    local key="$1" fname="$2"
    # Common: must contain the huc id and the exact cluster (cluster_208, not 2/20).
    [[ "$fname" == *"$HUC"* ]] || return 1
    [[ "$fname" =~ cluster_${CLUSTER}([^0-9]|$) ]] || return 1
    case "$key" in
        dem)
            [[ "$fname" == *wbt* ]] && return 1 ;;
        terr)
            [[ "$fname" == *slp* && "$fname" == *local* ]] || return 1
            [[ "$fname" =~ (10m|1000m) ]] && return 1 ;;
    esac
    return 0
}

echo "=============================================================="
echo " Syncing HUC source rasters"
echo "   cluster:     $CLUSTER"
echo "   huc:         $HUC"
echo "   from:        ${SERVER:-<local>}${REMOTE_ROOT}"
echo "   to:          $LOCAL_ROOT"
[ "$DRY_RUN" -eq 1 ] && echo "   mode:        DRY RUN (listing only)"
echo "=============================================================="

MATCHED=()      # relative paths (under the data root) to transfer
MISSING=()      # dataset keys with no match

for key in $KEYS; do
    sub="$(subdir_for "$key")"
    remote_dir="$REMOTE_ROOT/$sub"

    # List candidate .tif filenames in this dataset dir (names only, cheap).
    listing="$(run_remote "find '$remote_dir' -maxdepth 1 -type f -name '*.tif' 2>/dev/null" || true)"

    found=0
    if [ -n "$listing" ]; then
        while IFS= read -r path; do
            [ -n "$path" ] || continue
            fname="$(basename "$path")"
            if keep_file "$key" "$fname"; then
                MATCHED+=("$sub/$fname")
                found=$((found + 1))
                echo "  [$key] $fname"
            fi
        done <<< "$listing"
    fi

    if [ "$found" -eq 0 ]; then
        MISSING+=("$key")
        echo "  [$key] <no match>"
    fi
done

echo "--------------------------------------------------------------"
echo "Matched ${#MATCHED[@]} file(s) across $(echo $KEYS | wc -w | tr -d ' ') datasets."

if [ "${#MISSING[@]}" -ne 0 ]; then
    echo "WARNING: no source found for: ${MISSING[*]}"
    echo "         dl_huc_stack.py requires every dataset -- prediction will fail"
    echo "         for this HUC until these are present."
fi

if [ "${#MATCHED[@]}" -eq 0 ]; then
    echo "Nothing to transfer. Check CLUSTER/HUC and REMOTE_ROOT." >&2
    exit 1
fi

if [ "$DRY_RUN" -eq 1 ]; then
    echo "Dry run -- no files transferred."
    exit 0
fi

echo "Transferring..."
for rel in "${MATCHED[@]}"; do
    dest="$LOCAL_ROOT/$rel"
    mkdir -p "$(dirname "$dest")"
    # shellcheck disable=SC2086
    rsync $RSYNC_OPTS "${SERVER}${REMOTE_ROOT}/$rel" "$dest"
done

echo "=============================================================="
echo "Done. ${#MATCHED[@]} file(s) synced to $LOCAL_ROOT"
echo "Predict with:"
echo "  python dl_06b_predict_huc.py --huc $HUC --cluster $CLUSTER \\"
echo "      --data-root \"$LOCAL_ROOT\" --model Models/<model>.safetensors"
echo "=============================================================="
