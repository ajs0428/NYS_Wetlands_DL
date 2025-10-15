#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/Data/CHMs/AWS/
export PATH=/programs/AWS2:$PATH

# Array of directory names
directories=(
"chm_FEMA_OneidaSubbasin2016/"
"chm_NY_FEMAR2_Central_4_south_2018/"
"chm_NYSGPO_WarrenWashingtonEsssex_2015/"
)

# Loop through each directory name
for dir in "${directories[@]}"; do
    echo "Processing: $dir"
    
     # Create the directory if it doesn't exist
    if [ ! -d "$dir" ]; then
        echo "Creating directory: $dir"
        mkdir -p "$dir"
    else
        echo "Directory '$dir' already exists, syncing..."
    fi
    
    # Change into the directory
    cd "$dir"
    
    # Download from S3
    aws s3 sync "s3://cafri-share/$dir" .
    
    # Return to parent directory
    cd ..
    
    echo "Completed: $dir"
    echo "---"
done

echo "All downloads complete!"
