#!/bin/bash -l
set -euo pipefail

# rsync_push_v3.sh
#
# Push ONLY what a v3 factorial run needs from the CPU node (/ibstorage,
# canonical) to a GPU node's local /workdir. The counterpart to
# rsync_results.sh, which pulls results back.
#
# WHY THIS EXISTS: the blanket push in EXECUTION.md
#     rsync -av --exclude '.git' --exclude '.venv' <repo>/ <node>:/workdir/...
# sweeps ~458 GB, because the repo root carries every previous generation's
# outputs -- Models/ (276 GB of v1/v2/patchcurve/arch checkpoints) and
# Data/HUC_DL_Predictions_v2 (154 GB of inference GeoTIFFs). None of it is read
# by a v3 run: run_config.sh reads only $STATS_DIR + the three patch dirs, and
# every cell it writes is created fresh under Models/factorial_results_v3/.
# This script sends the ~14 GB that is actually load-bearing.
#
# WHAT GOES (verified against run_config.sh / dl_patch_pools.resolve_pools):
#   Python_Code_Analysis/DL_Pipeline_v2   pipeline + dl_experiment_config --emit
#   Shell_Scripts                         the run_*.sh drivers
#   Data/Training_Data/stats              the 16 per-config x per-mode stats
#   Data/Training_Data/R_Patches          field labels (TEST for every cell)
#   Data/Training_Data/R_Patches_NWI      NWI paired 1:1 to field
#   Data/Training_Data/R_Patches_NWIextra NWI at extra same-HUC12 locations
#   Data/NY_HUCS                          HUC12 boundaries for dl_07 masking
#
# WHAT STAYS: Models/ (all of it -- v3 cells are written on the node), the
# HUC_DL_Predictions_* roots, R_Patches_Merged* (v1/v2 pools), the *_v1/_v2
# stats dirs, the master stats JSONs (consumed by dl_make_config_stats on the
# CPU node only), the .venv, and the Docker image tarball (see --with-image:
# it belongs in /workdir/$USER, NOT inside the repo copy).
#
# Usage:
#   GPU_NODE=cbsugpu10.biohpc.cornell.edu ./rsync_push_v3.sh [-n] [--with-image]
#
#   -n | --dry-run  : list what would transfer, move nothing.
#   --with-image    : also push nys-wetlands-dl.tar.gz to /workdir/$USER/
#                     (its own transfer -- it is NOT part of the repo copy).
#   --delete        : prune remote files no longer present locally.
#
# Re-run before every reservation: rsync moves only what changed, and running a
# stale wrapper on the node is the classic silent failure here.

# === CONFIGURATION (override via environment) ===
GPU_NODE="${GPU_NODE:-cbsugpu10.biohpc.cornell.edu}"
REMOTE_USER="${REMOTE_USER:-$USER}"
REMOTE_ROOT="${REMOTE_ROOT:-/workdir/$REMOTE_USER/nys_wetlands}"
RSYNC_OPTS="${RSYNC_OPTS:--avhP}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# === ARGS ===
DRY_RUN=0
WITH_IMAGE=0
DELETE=0
for a in "$@"; do
    case "$a" in
        -n|--dry-run)  DRY_RUN=1 ;;
        --with-image)  WITH_IMAGE=1 ;;
        --delete)      DELETE=1 ;;
        -h|--help)     sed -n '3,45p' "${BASH_SOURCE[0]}"; exit 0 ;;
        *) echo "[error] unknown arg: $a" >&2; exit 2 ;;
    esac
done

# === THE PAYLOAD ===
# --relative recreates each path under REMOTE_ROOT, so <root>/X mounts as /app/X.
PATHS=(
    Python_Code_Analysis/DL_Pipeline_v2
    Shell_Scripts
    Data/Training_Data/stats
    Data/Training_Data/R_Patches
    Data/Training_Data/R_Patches_NWI
    Data/Training_Data/R_Patches_NWIextra
    # HUC12 boundaries (36 MB). Not a training input -- run_predict_factorial.sh
    # hands this to dl_07_mask_predictions.py to clip each prediction raster to
    # its watershed. Copied from the sibling NYS_Wetlands_Data project, which
    # stays canonical; refresh with:
    #   cp ../NYS_Wetlands_Data/Data/NY_HUCS/NY_Cluster_Zones_250_CROP_NAomit_6347.gpkg \
    #      Data/NY_HUCS/
    Data/NY_HUCS
)

EXCLUDES=(
    --exclude='__pycache__'
    --exclude='.ipynb_checkpoints'
    --exclude='*.pyc'
    --exclude='*.ckpt'          # no checkpoint is an input to a fresh v3 run
    --exclude='.DS_Store'
)

# === PROVENANCE ===
# .git is not pushed (it is large and useless on the node), but run_config.sh
# stamps every manifest.json with the commit -- without this file it would
# record "unknown" for all 100 cells. Materialize it here so the staged tree
# still carries its own provenance.
GIT_COMMIT="$(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"
GIT_DIRTY=""
git -C "$REPO_ROOT" diff --quiet 2>/dev/null || GIT_DIRTY="-dirty"
# Written even under -n: it is a local stamp with no remote effect, and it keeps
# the dry run's file list identical to what a real push would send.
printf '%s\n' "${GIT_COMMIT}${GIT_DIRTY}" > "$REPO_ROOT/.git_commit"
PATHS+=(.git_commit)

# === PREFLIGHT: fail loudly rather than staging a half-tree ===
for p in "${PATHS[@]}"; do
    [[ -e "$REPO_ROOT/$p" ]] || { echo "[error] missing locally: $p" >&2; exit 1; }
done
N_STATS="$(ls "$REPO_ROOT"/Data/Training_Data/stats/*_wp0.5.json 2>/dev/null | wc -l)"
echo "[info] node:    $REMOTE_USER@$GPU_NODE:$REMOTE_ROOT"
echo "[info] commit:  ${GIT_COMMIT}${GIT_DIRTY}"
echo "[info] stats:   $N_STATS files in Data/Training_Data/stats"
echo "[info] payload: $(du -shc --exclude=__pycache__ --exclude=.ipynb_checkpoints \
        $(printf "$REPO_ROOT/%s " "${PATHS[@]}") 2>/dev/null | tail -1 | cut -f1)"

OPTS=($RSYNC_OPTS --relative "${EXCLUDES[@]}")
[[ "$DRY_RUN" -eq 1 ]] && OPTS+=(-n)
[[ "$DELETE"  -eq 1 ]] && OPTS+=(--delete)

# === PUSH ===
ssh "$REMOTE_USER@$GPU_NODE" "mkdir -p '$REMOTE_ROOT' '/workdir/$REMOTE_USER/tmp'"

cd "$REPO_ROOT"
echo "+ rsync ${OPTS[*]} ${PATHS[*]} $REMOTE_USER@$GPU_NODE:$REMOTE_ROOT/"
rsync "${OPTS[@]}" "${PATHS[@]}" "$REMOTE_USER@$GPU_NODE:$REMOTE_ROOT/"

# === OPTIONAL: the Docker image tarball ===
# Goes to /workdir/$USER, one level ABOVE the repo copy -- it is an input to
# `docker1 load`, not something the container should see through the /app mount.
if [[ "$WITH_IMAGE" -eq 1 ]]; then
    IMG="$REPO_ROOT/nys-wetlands-dl.tar.gz"
    [[ -f "$IMG" ]] || { echo "[error] no image tarball at $IMG" >&2; exit 1; }
    IMG_OPTS=(-avhP)
    [[ "$DRY_RUN" -eq 1 ]] && IMG_OPTS+=(-n)
    echo "+ rsync ${IMG_OPTS[*]} $IMG -> /workdir/$REMOTE_USER/  ($(du -h "$IMG" | cut -f1))"
    rsync "${IMG_OPTS[@]}" "$IMG" "$REMOTE_USER@$GPU_NODE:/workdir/$REMOTE_USER/"
fi

echo "[done] staged. On the node:  docker1 run --rm --gpus all --shm-size=8g \\"
echo "         --user \$(id -u):\$(id -g) -v $REMOTE_ROOT:/app -e TMPDIR=/app/tmp \\"
echo "         nys-wetlands-dl bash Shell_Scripts/run_factorial.sh"
