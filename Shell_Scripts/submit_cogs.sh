#!/bin/bash
#SBATCH --job-name=cog_convert
#SBATCH --partition=R256C128
#SBATCH --array=0-59%12
#SBATCH --mem=24G
#SBATCH --cpus-per-task=4
#SBATCH --time=00:30:00
#SBATCH --output=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2/logs/cog_%A_%a.out
#SBATCH --error=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2/logs/cog_%A_%a.err

# --- Batch COG conversion for NYS wetland DL predictions -------------------
# Prereqs (run once, before submitting):
#   DATA_DIR=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2
#   ls -1 $DATA_DIR/*_probs.tif > $DATA_DIR/probs_list.txt   # absolute paths
#   mkdir -p $DATA_DIR/logs $DATA_DIR/cogs $DATA_DIR/frags
#   wc -l $DATA_DIR/probs_list.txt   # 60 -> --array=0-59 above must match
#
# %12 caps concurrency. Partition R256C128 = cbsuxu09+10 only, both of which
# have the wetland-cnn conda env under /workdir/$USER/miniconda3 (it is NOT
# visible from cbsuxu01-08, so don't widen the partition without a shared env).
# No --nodelist: with an array it would make every task allocate both nodes.
#
# Resumable: a task exits early if its COG + manifest fragment already exist,
# so re-submitting the same array after a partial failure only redoes the gaps.
# ---------------------------------------------------------------------------
set -euo pipefail

PROJECT_ROOT=/ibstorage/anthony/NYS_Wetlands_DL
INDIR=$PROJECT_ROOT/Data/HUC_DL_Predictions_v2
OUTDIR="$INDIR/cogs"
FRAGDIR="$INDIR/frags"
SCRIPT=$PROJECT_ROOT/Python_Code_Analysis/python_make_cogs.py
LIST="$INDIR/probs_list.txt"

# conda env with rasterio + rio-cogeo (node-local; see partition note above).
# conda's activate/deactivate hooks reference unset vars (libxml2), which is
# fatal under `set -u` — relax it just for activation.
set +u
source /workdir/$USER/miniconda3/etc/profile.d/conda.sh
conda activate wetland-cnn
set -u

mapfile -t FILES < "$LIST"
IN="${FILES[$SLURM_ARRAY_TASK_ID]:-}"

if [[ -z "$IN" ]]; then
  echo "No file at index $SLURM_ARRAY_TASK_ID (list has ${#FILES[@]} entries)" >&2
  exit 1
fi

STEM=$(basename "$IN" .tif)
COG="$OUTDIR/${STEM%_probs}_cog.tif"
FRAG="$FRAGDIR/manifest_${STEM}.json"
if [[ -s "$COG" && -s "$FRAG" ]]; then
  echo "Task $SLURM_ARRAY_TASK_ID: $COG already done, skipping"
  exit 0
fi

echo "Task $SLURM_ARRAY_TASK_ID -> $IN"
python "$SCRIPT" --inputs "$IN" --outdir "$OUTDIR" --frag-dir "$FRAGDIR"
