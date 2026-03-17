#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=32G
#SBATCH --cpus-per-task=5
#SBATCH --job-name=sat_gee_processing
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-sat-gee-%j.out
#SBATCH --time=48:00:00

ulimit -v

cd /ibstorage/anthony/NYS_Wetlands_DL/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3
# include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 208 218 225 250)
include=(208)
for number in "${include[@]}"; do
Rscript R_Code_Analysis/Sentinel_FromGEE_Processing.R \
    "Data/TerrainProcessed/HUC_DEMs/" \
    "Data/Satellite/GEE_Download_NY_HUC_Sentinel_Indices/ny_huc_indices" \
    "Data/Satellite/HUC_Processed_NY_Sentinel_Indices/" \
    "$number" >> "Shell_Scripts/logs/sat_gee_$(date +%Y%m%d).log" 2>&1 
	
done

echo "All Satellite GEE Processed Rscripts executions completed."

