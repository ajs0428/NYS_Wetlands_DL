#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/Wetland_Model_Prediction.r \
        208 \
	"coarse" \
	"Data/Predicted_Wetland_Rasters/" > Shell_Scripts/prediction_coarse.log 2>&1 &
echo $! > Shell_Scripts/prediction_coarse.pid
echo "R script for Wetland Model Prediction started with PID: $!"
echo "Monitor with: tail -f prediction_coarse.log"
