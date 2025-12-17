#!/bin/bash -l
#SBATCH --nodelist=cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=8
#SBATCH --job-name=rast_check
#SBATCH --ntasks=1
#SBATCH --output=Shell_Scripts/SLURM/slurm-corr-%j.out

cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

# Define the list of numbers
# include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 208 218 225 250)
include=(67 120 123 189 198)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    R_Code_Analysis/raster_pre_model_checklist.R \
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg" \
    "$number" \
     "Data/CHMs/HUC_CHMs/" >> "Shell_Scripts/logs/rast_check_$(date +%Y%m%d).log" 2>&1 
    
done

echo "All checklist Rscripts executions completed."

