#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/

scale_arg=${2:-"NS"} 

echo "Name of script is: $0" # Prints the name of this script 
echo "Directory to check for missing HUCs: $1" # Prints the name of the directory in the first command line argument
echo "Terrain (optional): $scale_arg"

missname=$(basename "$1") # Extract just the directory name from the full path and store in variable "missname"
echo "$missname"

# Print the output filepath where all found HUCs will be saved
echo "All HUCs in: Data/Dataframes/HUCs_in_${missname}_folder.txt"
# Print the output filepath where missing HUCs will be saved
echo "Missing HUCs in: Data/Dataframes/Missing_HUCs_in_${missname}_${scale_arg}_to_check.txt"

# List files in directory, extract HUC numbers after "huc_", sort numerically/uniquely, save to file
if [[ "$scale_arg" == "NS" ]]; then
    # If no scale specified, match any HUC pattern
    ls "$1" | grep -oP "huc_\K\d+(?=_)" | sort -nu > Data/Dataframes/HUCs_in_${missname}_folder.txt
else
    # If scale specified, match HUCs with that scale suffix
    ls "$1" | grep -oP "huc_\K\d+(?=_terrain.*_${scale_arg})" | sort -nu > Data/Dataframes/HUCs_in_${missname}_folder.txt
fi

# Find HUCs in master list: Data/Dataframes/HUCs_in_site_clusters_NAomit.txt that aren't in the folder list, save missing ones to file
grep -Fxvf Data/Dataframes/HUCs_in_${missname}_folder.txt Data/Dataframes/HUCs_in_site_clusters_NAomit.txt > \
    Data/Dataframes/Missing_HUCs_in_${missname}_${scale_arg}_to_check.txt