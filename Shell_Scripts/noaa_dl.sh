#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/Data/NAIP/noaa_digital_coast_2017

wget -w 0.5 -i /ibstorage/anthony/NYS_Wetlands_GHG/Shell_Scripts/noaa_NY_NAIP_2017.txt

echo "All downloads complete!"