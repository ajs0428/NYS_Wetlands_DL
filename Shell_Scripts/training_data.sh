#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/TrainingDataGenerationECO_CMD.r \
        "Data/NWI/NY_Wetlands_6350_filterTrain.gpkg" \
	"Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
	"cluster" > Shell_Scripts/training_data.log 2>&1 &
echo $! > training_data.pid
echo "R script for training data generation started with PID: $!"
echo "Monitor with: tail -f training_data.log"



