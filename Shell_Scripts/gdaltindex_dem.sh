#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/Data/DEMs/
export PATH=/programs/gdal-3.5.2/bin:$PATH
export LD_LIBRARY_PATH=/programs/gdal-3.5.2/lib

# Array of directory names
directories=(
    "USGS_LakeOntarioHudsonRiverRegion2022"
)

# Loop through each directory name
for dir in "${directories[@]}"; do
    echo "Processing: $dir"

    # Define the output .gpkg filename
    gpkg_file="${dir}.gpkg"

    # Create the index file if it doesn't exist
    if [ ! -f "$gpkg_file" ]; then
        echo "Creating index: $gpkg_file"
        find "$dir" \( -name "*.tif" -o -name "*.tiff" \) -print0 | xargs -0 gdaltindex "$gpkg_file"
        mv "$gpkg_file" /ibstorage/anthony/NYS_Wetlands_GHG/Data/NYS_DEM_Indexes/
    else
        echo "Index '$gpkg_file' already exists, skipping..."
    fi

    echo "Completed: $dir"
    echo "---"
done

echo "All processing complete!"