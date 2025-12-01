#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=16
#SBATCH --job-name=training_data_gen
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-train-%j.out

cd /ibstorage/anthony/NYS_Wetlands_GHG

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3


Rscript R_Code_Analysis/TrainingDataGenerationFlex_CMD.R \
    "Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYSWetlands_NYNHP_NatComm_data_combined.gpkg" \
	"Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg" \
	"cowardin" \
	"cluster" > "Shell_Scripts/logs/training_data_gen_$(date +%Y%m%d).log" 2>&1





