#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=128G
#SBATCH --cpus-per-task=1
#SBATCH --job-name=hydro
#SBATCH --ntasks=1
#SBATCH --output=Shell_Scripts/SLURM/slurm-hydro-%j.out


cd /ibstorage/anthony/NYS_Wetlands_DL/

export TMPDIR=/ibstorage/anthony/NYS_Wetlands_DL/Data/tmp/

module load R/4.4.3


# Define the list of numbers
include=(22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 208 218 225 250 11 12)
# include=(11 12 67 208 123)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    Rscript R_Code_Analysis/hydro_metrics_singleVect_CMD.r \
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg" \
    "$number" \
    "Data/TerrainProcessed/HUC_DEMs/" \
    "Data/TerrainProcessed/HUC_Hydro/" >> "Shell_Scripts/logs/hydro_$(date +%Y%m%d).log" 2>&1 
    
done

echo "All Rscript executions completed."

