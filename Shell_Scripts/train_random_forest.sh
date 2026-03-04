#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=12G
#SBATCH --cpus-per-task=8
#SBATCH --job-name=train_random_forest
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-trainRF-%j.out

ulimit -v

cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

Rscript R_Code_Analysis/train_random_forest.R \
	"Data/Training_Data/HUC_Extracted_Training_Data/" \
	"MOD_CLASS" \
	"Models/RF_model_output" >> "Shell_Scripts/logs/train_RF_$(date +%Y%m%d).log" 2>&1