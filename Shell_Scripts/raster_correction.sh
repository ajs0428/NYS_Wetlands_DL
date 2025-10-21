#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/raster_data_correction.r \
        "Data/TerrainProcessed/HUC_Hydro/" \
	208 > Shell_Scripts/raster_correction.log 2>&1 &
echo $! > Shell_Scripts/raster_correction.pid
echo "R script for raster correction started with PID: $!"
echo "Monitor with: tail -f raster_correction.log"
