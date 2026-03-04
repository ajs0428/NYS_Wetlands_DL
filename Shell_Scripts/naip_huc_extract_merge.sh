#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/NAIP_HUC_Merging_CMD.r \
	"Data/NY_HUCS/NY_Cluster_Zones_200.gpkg" \
	208 \
	"Data/NAIP/NAIP_Processed/" \
	"Data/NAIP/NAIP_HUC_Merged/" > Shell_Scripts/naip_huc_extract_merge.log 2>&1 &
echo $! > Shell_Scripts/naip_huc_extract_merge.sh.pid
echo "R script for naip huc and merge started with PID: $!"
echo "Monitor with: tail -f naip_huc_extract_merge.log"




