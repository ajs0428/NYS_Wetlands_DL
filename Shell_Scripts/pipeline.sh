#!/bin/bash -l


cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

# Starting with making HUC terrain data

Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        208 \
	"Data/TerrainProcessed/HUC_DEMs" \
        "slp" \
        "Data/TerrainProcessed/HUC_TerrainMetrics/" > Shell_Scripts/terrain.log 2>&1 &
echo $! > terrain.pid
echo "R script for SLP started with PID: $!"
echo "Monitor with: tail -f terrain.log"

wait

Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        208 \
	"Data/TerrainProcessed/HUC_DEMs" \
        "dmv" \
        "Data/TerrainProcessed/HUC_TerrainMetrics/" > Shell_Scripts/terrain.log 2>&1 &
echo $! > terrain.pid
echo "R script for DMV started with PID: $!"
echo "Monitor with: tail -f terrain.log"

wait


Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        208 \
	"Data/TerrainProcessed/HUC_DEMs" \
        "curv" \
        "Data/TerrainProcessed/HUC_TerrainMetrics/" > Shell_Scripts/terrain.log 2>&1 &
echo $! > terrain.pid
echo "R script for CURVE started with PID: $!"
echo "Monitor with: tail -f terrain.log"


wait


cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/raster_data_extraction.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        208 \
	"Data/TerrainProcessed/HUC_TerrainMetrics/" \
        "Data/TerrainProcessed/HUC_Hydro/" \
        "Data/NAIP/NAIP_HUC_Merged/" \
        "no" \
	"Data/Training_Data/Cluster_Extract_Training_Pts/" > Shell_Scripts/training_pts_extract.log 2>&1 &
echo $! > Shell_Scripts/training_pts_extract.pid
echo "R script for training points extraction started with PID: $!"
echo "Monitor with: tail -f training_pts_extract.log"








