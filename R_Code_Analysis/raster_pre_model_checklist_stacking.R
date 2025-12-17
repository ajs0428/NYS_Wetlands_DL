#!/usr/bin/env Rscript

#################
# This script checks the predictor layer rasters before 
# they are used in data extraction, modeling, and prediction
#################

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg", #1
    67, #2
    "Data/TerrainProcessed/HUC_TerrainMetrics/", #3
    "Data/TerrainProcessed/HUC_Hydro/", #4
    "Data/NAIP/HUC_NAIP_Processed/", #5
    "Data/CHMs/HUC_CHMs/", #6
    "Data/Training_Data/Cluster_Extract_Training_Pts/" #7
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area:", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path Terrain Metrics", args[3], "\n",
    "- Path to Hydrology Metrics:", args[4], "\n",
    "- Path to NAIP Imagery:", args[5], "\n",
    "- Path to CHMs:", args[6], "\n",
    "- Path to Export:", args[7], "\n"
)


###############################################################################################
library(terra)
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))


terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())

###############################################################################################
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)

huc_names <- cluster_target$huc12 |> unique()
###############################################################################################

dem_list <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = paste0("cluster_", args[2], "_", ".*\\d+.tif"), full.names = TRUE)
terr_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], "_", ".*\\m.tif"), full.names = TRUE) %>% 
    .[!str_detect(., "1000m|10m")]
hydro_list <- list.files(path = args[4], pattern = paste0("cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
naip_list <- list.files(path = args[5], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
chm_list <- list.files(path = args[6], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)


bad_list <- c()
for(i in huc_names){
    tryCatch({
        c(rast(dem_list[str_detect(dem_list, i)]),
          rast(terr_list[str_detect(terr_list, i)]),
          rast(naip_list[str_detect(naip_list, i)]), 
          rast(chm_list[str_detect(chm_list, i)]),
          rast(hydro_list[str_detect(hydro_list, i)])
        ) }, 
        error = function(msg){
            message(msg$message)
            message(paste0("Error at: ", i))
            # bad_list[[length(bad_list) + 1]] <<- i
            bad_list <<- c(bad_list, i)
            return(NA)
        })
}

if(is.null(bad_list)){
    bad_list <- c(bad_list, "None")
} else {
    bad_list <- bad_list
}

bad_df <- data.frame("Date" = Sys.Date(),
                     "cluster" = args[2], 
                     "bad_huc_stacks" = bad_list)
readr::write_csv(bad_df, append = TRUE, file = paste0("Data/Dataframes/Non_Stacking_HUCs_", str_replace(Sys.Date(), "-", "_"), ".csv"), 
                 col_names = T)



# read.csv("Data/Dataframes/Non_Stacking_HUCs_2025_12-03.csv")
# 
# rast(dem_list[str_detect(dem_list, "020200031103")])
# rast(terr_list[str_detect(terr_list, "020200031103")])
# rast(naip_list[str_detect(naip_list, "020200031103")])
# rast(chm_list[str_detect(chm_list, "020200031103")])
# rast(hydro_list[str_detect(hydro_list, "020200031103")])
