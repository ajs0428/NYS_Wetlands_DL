#!/bin/bash -l

cd /ibstorage/anthony/NYS_Wetlands_GHG/Data/CHMs/AWS/
export PATH=/programs/gdal-3.5.2/bin:$PATH
export LD_LIBRARY_PATH=/programs/gdal-3.5.2/lib

# Array of directory names
directories=(
"chm_FEMA_FranklinStLawrence2016"
"chm_FEMA_FultonSaratogaHerkimerFranklin2017"
"chm_FEMA_GreatLakes2014"
"chm_FEMA_HudsonHoosic2012"
"chm_FEMA_OneidaSubbasin2016"
"chm_FEMA_OniedaSubbasin2016"
"chm_NY_FEMAR2_Central_2_2018"
"chm_NY_FEMAR2_Central_3_2018"
"chm_NY_FEMAR2_Central_4_2018_north"
"chm_NY_FEMAR2_Central_4_south_2018"
"chm_NY_FEMAR2_Central_5_2018"
"chm_NY_FEMAR2_Central_B1_2018_east"
"chm_NY_FEMAR2_Central_B1_2018_west"
"chm_NYSGPO_AlleganySteuben2016"
"chm_NYSGPO_CayugaOswego_2018"
"chm_NYSGPO_CentralFingerLakes_2020"
"chm_NYSGPO_ColumbiaRensselaer2016"
"chm_NYSGPO_ErieGeneseeLivingston2019"
"chm_NYSGPO_GreatGully2014"
"chm_NYSGPO_MadisonOtsego_2015"
"chm_NYSGPO_SouthwestB_fall_2017"
"chm_NYSGPO_Southwest_spring_2017"
"chm_NYSGPO_WarrenWashingtonEsssex_2015"
"chm_USGS_3County2014"
"chm_USGS_ClintonEssexFranklin2014"
"chm_USGS_LongIsland2014"
"chm_USGS_NorthEast2011"
"chm_USGS_NYC2014"
"chm_USGS_Schoharie2014"
)

# Loop through each directory name
for dir in "${directories[@]}"; do
    echo "Processing: $dir"
    
    # Define the output .gpkg filename
    gpkg_file="${dir}.gpkg"
    
    # Create the index file if it doesn't exist
    if [ ! -f "$gpkg_file" ]; then
        echo "Creating index: $gpkg_file"
        find "$dir" -name "*.tiff" -print0 | xargs -0 gdaltindex "$gpkg_file"
    else
        echo "Index '$gpkg_file' already exists, skipping..."
    fi
    
    echo "Completed: $dir"
    echo "---"
done

echo "All downloads complete!"
