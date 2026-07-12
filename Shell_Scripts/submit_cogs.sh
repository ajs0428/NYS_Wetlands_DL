#!/bin/bash
#SBATCH --job-name=cog_convert
#SBATCH --array=0-119%12
#SBATCH --mem=16G
#SBATCH --cpus-per-task=4
#SBATCH --time=00:30:00
#SBATCH --output=logs/cog_%A_%a.out
#SBATCH --error=logs/cog_%A_%a.err

# --- Batch COG conversion for NYS wetland DL predictions -------------------
# Prereqs (run once, before submitting):
#   ls -1 /ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2/*_probs.tif > probs_list.txt
#   mkdir -p logs cogs frags
#   wc -l probs_list.txt   # confirm this matches --array range (0 .. N-1)
#
# Adjust the --array upper bound to (number_of_files - 1). %12 caps concurrency;
# lower it if the node RAM can't hold 12 input stacks at once.
# ---------------------------------------------------------------------------

INDIR=/ibstorage/anthony/NYS_Wetlands_DL/Data/HUC_DL_Predictions_v2
OUTDIR="$INDIR/cogs"
FRAGDIR="$INDIR/frags"

mapfile -t FILES < probs_list.txt
IN="${FILES[$SLURM_ARRAY_TASK_ID]}"

if [[ -z "$IN" ]]; then
  echo "No file at index $SLURM_ARRAY_TASK_ID" >&2
  exit 1
fi

echo "Task $SLURM_ARRAY_TASK_ID -> $IN"
python make_cogs.py --inputs "$IN" --outdir "$OUTDIR" --frag-dir "$FRAGDIR"