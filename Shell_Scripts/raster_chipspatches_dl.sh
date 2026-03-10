#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=24G
#SBATCH --cpus-per-task=4
#SBATCH --job-name=raster_patch
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-patch-%j.out


cd /ibstorage/anthony/NYS_Wetlands_DL/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

# Define the list of numbers
include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
# include=(11 152 64 208)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    Rscript R_Code_Analysis/Raster_ChipsPatches_DL.R \
    "Data/Training_Data/R_Patches_Vector_Reviewed/" \
    128 >> "Shell_Scripts/logs/raster_patch_$(date +%Y%m%d).log" 2>&1
    
done

echo "All Rscript executions completed."

