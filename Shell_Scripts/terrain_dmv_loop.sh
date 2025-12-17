#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=48G
#SBATCH --cpus-per-task=3
#SBATCH --job-name=dmv
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-dmv-%j.out


cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3


# Define the list of numbers
#include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
include=(67 120 123)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg" \
    "$number" \
    "Data/TerrainProcessed/HUC_DEMs" \
    "dmv" \
    "Data/TerrainProcessed/HUC_TerrainMetrics/" >> "Shell_Scripts/logs/terrain_dmv_$(date +%Y%m%d).log" 2>&1
    
done

echo "All Rscript executions completed."

