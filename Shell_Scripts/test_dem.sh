#!/bin/bash/ -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3


Rscript R_Code_Analysis/DEM_Extract_singleVect_CMD.r "Data/NYS_DEM_Indexes/" "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        208 "Data/DEMs/" "Data/TerrainProcessed" >& test_dem.log &
