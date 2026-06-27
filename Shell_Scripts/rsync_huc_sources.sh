#!/bin/bash -l
set -euo pipefail

# rsync_huc_sources.sh
#
# Sync the predictor SOURCE rasters for ONE OR MORE HUCs (each a cluster + huc
# id) from the server where they live to a local data-root, mirroring the
# directory layout that dl_huc_stack.py / dl_06b_predict_huc.py expect. This
# lets you run mapped predictions from in-memory stacks (no *_stack.tif) without
# copying hundreds of GB -- you pull only the handful of files per HUC.
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
#     ./rsync_huc_sources.sh [-n|--dry-run] <PAIRS...>
#
# A PAIR ties one cluster to one huc id; pass as many as you like. Three forms,
# freely mixable:
#   1. Legacy two positional args (one pair):   208 041402011002
#   2. Inline colon pairs (any number):         208:041402011002 209:030102040506
#   3. A manifest file:                         --pairs hucs.txt
#      (one pair per line, separated by space/comma/colon; '#' comments and
#       blank lines ignored, e.g.  "208 041402011002"  or  "208,041402011002")
#
# Examples:
#   SERVER="ajs@greene.hpc.nyu.edu:" REMOTE_ROOT="/scratch/ajs/NYS_Wetlands_Data" \
#   LOCAL_ROOT="/data/NYS_Wetlands_Data" \
#     ./rsync_huc_sources.sh 208 041402011002                       # one HUC
#     ./rsync_huc_sources.sh 208:041402011002 209:030102040506      # several
#     ./rsync_huc_sources.sh --pairs hucs.txt                       # from file
#
# Then predict (per huc/cluster):
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
PAIRS=()        # each element is "CLUSTER HUC"
BAREARGS=()     # positional tokens without a ':' (legacy CLUSTER HUC form)
PAIRS_FILE=""

# Append "CLUSTER HUC" to PAIRS, validating both fields are non-empty.
add_pair() {
    local cl="$1" hc="$2"
    if [ -z "$cl" ] || [ -z "$hc" ]; then
        echo "ERROR: malformed pair (cluster='$cl' huc='$hc')" >&2
        exit 1
    fi
    PAIRS+=("$cl $hc")
}

# Read pairs from a manifest file: one pair per line, fields split on any of
# space / comma / colon; '#' comments and blank lines ignored.
read_pairs_file() {
    local file="$1" line cl hc
    [ -f "$file" ] || { echo "ERROR: --pairs file not found: $file" >&2; exit 1; }
    while IFS= read -r line || [ -n "$line" ]; do
        line="${line%%#*}"                      # strip trailing comment
        line="$(echo "$line" | tr ',:' '  ')"   # commas/colons -> spaces
        # shellcheck disable=SC2086
        set -- $line                            # word-split on whitespace
        [ "$#" -eq 0 ] && continue              # blank/comment-only line
        if [ "$#" -ne 2 ]; then
            echo "ERROR: bad line in $file (expected 'cluster huc'): $line" >&2
            exit 1
        fi
        add_pair "$1" "$2"
    done < "$file"
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        -n|--dry-run) DRY_RUN=1 ;;
        -h|--help)
            grep -E '^#( |$)' "$0" | sed -E 's/^# ?//'
            exit 0 ;;
        --pairs)
            shift
            [ "$#" -ge 1 ] || { echo "ERROR: --pairs needs a file argument" >&2; exit 1; }
            PAIRS_FILE="$1" ;;
        --pairs=*) PAIRS_FILE="${1#*=}" ;;
        *:*)  # inline cluster:huc pair
            add_pair "${1%%:*}" "${1#*:}" ;;
        *)    BAREARGS+=("$1") ;;
    esac
    shift
done

[ -n "$PAIRS_FILE" ] && read_pairs_file "$PAIRS_FILE"

# Bare positional tokens are taken pairwise (cluster huc cluster huc ...), which
# preserves the original two-arg invocation.
if [ "${#BAREARGS[@]}" -gt 0 ]; then
    if [ $(( ${#BAREARGS[@]} % 2 )) -ne 0 ]; then
        echo "ERROR: positional args must come in CLUSTER HUC pairs (got ${#BAREARGS[@]})" >&2
        exit 1
    fi
    i=0
    while [ "$i" -lt "${#BAREARGS[@]}" ]; do
        add_pair "${BAREARGS[$i]}" "${BAREARGS[$((i + 1))]}"
        i=$((i + 2))
    done
fi

if [ "${#PAIRS[@]}" -eq 0 ]; then
    echo "Usage: $(basename "$0") [-n|--dry-run] <CLUSTER HUC | cluster:huc ... | --pairs FILE>" >&2
    echo "  (set SERVER, REMOTE_ROOT, LOCAL_ROOT via environment)" >&2
    exit 1
fi

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

# Sync the sources for ONE pair. Sets the globals CLUSTER/HUC (read by
# keep_file), then lists + transfers matches. Returns 1 if nothing matched so
# the caller can record the failure and move on to the next HUC.
sync_pair() {
    CLUSTER="$1"        # global: consumed by keep_file
    HUC="$2"            # global: consumed by keep_file
    local matched=() missing=() key sub remote_dir listing found path fname rel dest

    echo "=============================================================="
    echo " Syncing HUC source rasters"
    echo "   cluster:     $CLUSTER"
    echo "   huc:         $HUC"
    echo "   from:        ${SERVER:-<local>}${REMOTE_ROOT}"
    echo "   to:          $LOCAL_ROOT"
    [ "$DRY_RUN" -eq 1 ] && echo "   mode:        DRY RUN (listing only)"
    echo "=============================================================="

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
                    matched+=("$sub/$fname")
                    found=$((found + 1))
                    echo "  [$key] $fname"
                fi
            done <<< "$listing"
        fi

        if [ "$found" -eq 0 ]; then
            missing+=("$key")
            echo "  [$key] <no match>"
        fi
    done

    echo "--------------------------------------------------------------"
    echo "Matched ${#matched[@]} file(s) across $(echo $KEYS | wc -w | tr -d ' ') datasets."

    if [ "${#missing[@]}" -ne 0 ]; then
        echo "WARNING: no source found for: ${missing[*]}"
        echo "         dl_huc_stack.py requires every dataset -- prediction will fail"
        echo "         for this HUC until these are present."
    fi

    if [ "${#matched[@]}" -eq 0 ]; then
        echo "Nothing to transfer for cluster=$CLUSTER huc=$HUC. Check CLUSTER/HUC and REMOTE_ROOT." >&2
        return 1
    fi

    if [ "$DRY_RUN" -eq 1 ]; then
        echo "Dry run -- no files transferred for this HUC."
        return 0
    fi

    echo "Transferring..."
    for rel in "${matched[@]}"; do
        dest="$LOCAL_ROOT/$rel"
        mkdir -p "$(dirname "$dest")"
        # shellcheck disable=SC2086
        rsync $RSYNC_OPTS "${SERVER}${REMOTE_ROOT}/$rel" "$dest"
    done
    GRAND_TOTAL=$((GRAND_TOTAL + ${#matched[@]}))

    echo "Done. ${#matched[@]} file(s) synced for cluster=$CLUSTER huc=$HUC"
    echo "  predict: python dl_06b_predict_huc.py --huc $HUC --cluster $CLUSTER \\"
    echo "             --data-root \"$LOCAL_ROOT\" --model Models/<model>.safetensors"
    return 0
}

# === DRIVE OVER ALL PAIRS ===
echo "Processing ${#PAIRS[@]} HUC pair(s)."
GRAND_TOTAL=0
FAILED=()
for pair in "${PAIRS[@]}"; do
    # shellcheck disable=SC2086
    set -- $pair                       # split "CLUSTER HUC"
    if ! sync_pair "$1" "$2"; then
        FAILED+=("$1:$2")
    fi
done

echo "=============================================================="
if [ "$DRY_RUN" -eq 1 ]; then
    echo "Dry run complete over ${#PAIRS[@]} pair(s) -- no files transferred."
else
    echo "Done. $GRAND_TOTAL file(s) synced to $LOCAL_ROOT across ${#PAIRS[@]} pair(s)."
fi
if [ "${#FAILED[@]}" -ne 0 ]; then
    echo "WARNING: no files matched for ${#FAILED[@]} pair(s): ${FAILED[*]}" >&2
    exit 1
fi
echo "=============================================================="
