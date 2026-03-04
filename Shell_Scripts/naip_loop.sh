#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=64G
#SBATCH --cpus-per-task=2
#SBATCH --job-name=naip
#SBATCH --ntasks=2
#SBATCH --output=Shell_Scripts/SLURM/slurm-naip-%j.out


cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3

# Define the list of numbers
# include=(11 12 22 51 53 56 60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 208 218 225 250)
include=(1 7 16 18 23 28 39 40 42 46 47 52 68 70 72 73 78 82 83 94 96 98 107 113 114 128 129 133 135 139 140 148 154 156 165 168 173 178 190 193 200 221 222 223 228 231 242 245 246)
# Loop through each number in the list
for number in "${include[@]}"; do
  echo "Running Rscript with argument: $number"
  Rscript R_Code_Analysis/NAIP_Processing_CMD.R \
  "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg" \
  "$number" \
  "Data/NAIP/HUC_NAIP_Processed/" >> "Shell_Scripts/logs/naip_$(date +%Y%m%d).log" 2>&1
  
done

echo "All Rscript executions completed."

