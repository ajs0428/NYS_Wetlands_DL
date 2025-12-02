#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=32G
#SBATCH --cpus-per-task=4
#SBATCH --job-name=training_data_extract
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-trainext-%j.out

ulimit -v

cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3


include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
# include=(208)


for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    Rscript R_Code_Analysis/training_pts_extract.r \
	"Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg" \
	"$number" \
	"Data/TerrainProcessed/HUC_TerrainMetrics/" \
	"Data/TerrainProcessed/HUC_Hydro/" \
	"Data/NAIP/HUC_NAIP_Processed/" \
	"Data/CHMs/HUC_CHMs/" \
	"Data/Training_Data/Cluster_Extract_Training_Pts/" >> "Shell_Scripts/logs/training_data_extract_$(date +%Y%m%d).log" 2>&1 
	
done

echo "All Training Data Extract Rscripts executions completed."

