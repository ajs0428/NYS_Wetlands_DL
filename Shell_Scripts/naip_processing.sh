#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG
module load R/4.4.3

Rscript R_Code_Analysis/NAIP_Processing_CMD.r \
         "Data/NAIP/" \
         "Data/NAIP/NAIP_Processed/" > Shell_Scripts/naip_processing.log 2>&1 &
echo $! > Shell_Scripts/naip_processing.pid
echo "R script for naip processing started with PID: $!"
echo "Monitor with: tail -f naip_processing.log"
