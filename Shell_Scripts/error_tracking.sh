#!/bin/bash -l


cd /ibstorage/anthony/NYS_Wetlands_GHG

while IFS= read -r filepath; do
    if [ -f "$filepath" ]; then
        echo "Found: $filepath"
        found_files+=("$filepath")
    else
        echo "Not found: $filepath"
        missing_files+=("$filepath")
    fi
done < Data/ErrorTracking/HUC_TerrainProcessed_bad_results.txt


if [ ${#found_files[@]} -gt 0 ]; then
    read -p "Do you want to delete these ${#found_files[@]} file(s)? (yes/no): " confirmation
    
    if [ "$confirmation" = "yes" ] || [ "$confirmation" = "y" ]; then
        echo ""
        echo "=== DELETING FILES ==="
        for filepath in "${found_files[@]}"; do
            rm "$filepath"
            echo "Removed: $filepath"
        done
        echo ""
        echo "Done! Deleted ${#found_files[@]} file(s)."
    else
        echo "Deletion cancelled."
    fi
else
    echo "No files to delete."
fi