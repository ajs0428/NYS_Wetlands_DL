#!/bin/bash -l
#SBATCH --nodelist=cbsuxu09,cbsuxu10
#SBATCH --mail-user=ajs544@cornell.edu
#SBATCH --mem=16G
#SBATCH --job-name=dems
#SBATCH --ntasks=1


cd /ibstorage/anthony/NYS_Wetlands_GHG/

export TMPDIR=/ibstorage/anthony/tmp

module load R/4.4.3


# Define the list of numbers
# include=(11  12  22  51  53  56#  60  64  67  84  86  90  92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
include=(60 64 67 84 86 90 92 102 105 116 120 123 136 138 152 176 183 189 192 193 198 218 225 250)
# Loop through each number in the list
for number in "${include[@]}"; do
    echo "Running Rscript with argument: $number"
    
    Rscript R_Code_Analysis/DEM_Extract_singleVect_CMD.r \
        "Data/NYS_DEM_Indexes/" \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        "$number" \
        "Data/DEMs/" \
        "Data/TerrainProcessed/HUC_DEMs/" >> "Shell_Scripts/dem_processing_loop.log" 2>&1
    
    if [ $? -eq 0 ]; then
        echo "Successfully completed Rscript for number: $number"
    else
        echo "Error: Rscript failed for number: $number"
    fi
    echo "---"
done


