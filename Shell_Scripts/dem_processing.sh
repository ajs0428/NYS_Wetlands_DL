#!/bin/bash -l
#SBATCH --nodes=2
#SBATCH --exclude=cbsuxufs1
#SBATCH --ntasks=3
#SBATCH --mem=128000
#SBATCH --time=2-23:00:00
#SBATCH --partition=regular,long30
#SBATCH --chdir=/ibstorage/anthony/NYS_Wetlands_GHG
#SBATCH --job-name=test_terrain
#SBATCH --output=jobname.out.%j
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL

cd /ibstorage/anthony/NYS_Wetlands_GHG/
module load R/4.4.3


exclude=(5 12 18 35 49 53 67 72 73 76 82 99 105 107 112 123 124 146 148 149 157 165 166 177)

for i in {200..210}; do
  if [[ ! " ${exclude[@]} " =~ " $i " ]]; then
    echo $i
	Rscript R_Code_Analysis/terrain_metrics_singleVect_CMD.r "Data/NYS_DEM_Indexes/" "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
	$i "Data/DEMs/" "Data/TerrainProcessed" &
  fi
done

