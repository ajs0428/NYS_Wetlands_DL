#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=32G
#SBATCH --cpus-per-task=3
#SBATCH --job-name=chm_var
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-chm-var-%j.out
#SBATCH --time=48:00:00

ulimit -v

cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

Rscript R_Code_Analysis/CHM_Variation_Indices.R \
    "Data/CHMs/HUC_CHMs/" \
    "Data/CHMs/HUC_CHMvar/" >> "Shell_Scripts/logs/chm_var_(date +%Y%m%d).log" 2>&1 
	
done

echo "All CHM Variance Processed Rscripts executions completed."

