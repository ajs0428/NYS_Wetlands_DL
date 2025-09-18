#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG

module load R/4.4.3

include=( 11 12 )

for i in {1..250}; do
  if [[ " ${include[@]} " =~ " $i " ]]; then
    echo "Processing cluster $i"
    
    # Run SLP script
    echo "Starting R script for SLP cluster $i"
    Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        $i \
        "Data/TerrainProcessed/HUC_DEMs" \
        "slp" \
        "Data/TerrainProcessed/HUC_TerrainMetrics/" > "Shell_Scripts/terrain_slp_${i}.log" 2>&1 &
    
    slp_pid=$!
    echo $slp_pid
    echo "SLP script for cluster $i started with PID: $slp_pid"
    
    # Wait for SLP script to complete
    wait $slp_pid
    echo "SLP script for cluster $i completed"
    
    # Run DMV script
    echo "Starting R script for DMV cluster $i"
    Rscript R_Code_Analysis/terrain_metrics_filter_singleVect_CMD.r \
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
        $i \
        "Data/TerrainProcessed/HUC_DEMs" \
        "dmv" \
        "Data/TerrainProcessed/HUC_TerrainMetrics/" > "Shell_Scripts/terrain_dmv_${i}.log" 2>&1 &
    
    dmv_pid=$!
    echo $dmv_pid 
    echo "DMV script for cluster $i started with PID: $dmv_pid"
    
    # Wait for DMV script to complete
    wait $dmv_pid
    echo "DMV script for cluster $i completed"
    
    echo "Both scripts for cluster $i finished"
    echo "---"
  fi
done

echo "All terrain metrics processing complete!"