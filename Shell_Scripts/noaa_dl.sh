#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/Data/NAIP/noaa_digital_coast/

wget -w 0.5 -i /ibstorage/anthony/NYS_Wetlands_GHG/Shell_Scripts/noaa_NY_NAIP.txt

echo "All downloads complete!"