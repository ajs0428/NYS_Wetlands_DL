#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
    208,
    "Data/NAIP/HUC_NAIP_Processed/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
    "- Path to cluster:", args[1], "\n",
    "- Cluster:", args[2], "\n",
    "- Path to NAIP Processed:", args[3], "\n"
))

###############################################################################################

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(foreach)
library(future)
library(future.apply)
library(parallel)
library(doParallel)

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())
setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files
###############################################################################################

#Index of all GEE NAIP tiles that need to be merged 
naip_index <- list.files("Data/NAIP/GEE_NAIP/ny_huc_naip_indices/", full.names = TRUE, pattern = ".tif")

l_dem <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".tif", full.names = TRUE) 
l_dem_cluster <- l_dem[str_detect(l_dem, paste0("cluster_", args[2])) & !str_detect(l_dem, "wbt")]

cluster_target <- sf::st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE,
                              query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE cluster = '", args[2], "'"))
huc_list <- cluster_target$huc12
###############################################################################################

process_huc <- function(huc) {
    target_file <- paste0(args[3], "cluster_", args[2], "_huc_", huc, "_NAIP_metrics.tif")
    naip_files <- naip_index[grepl(huc, naip_index)]
    dem_file <- l_dem_cluster[grepl(huc, l_dem_cluster)] 
    huc_poly <- sf::st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE,
                            query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE huc12 = '", huc, "'")) |> 
        vect()
    
    # uncomment the if statement with file.exists to ignore files already created
    #if(!file.exists(target_file)){
    print("no NAIP processed yet")
    
    naip_rast <- naip_files |> 
        lapply(FUN = terra::rast) |> 
        terra::sprc() |> 
        terra::mosaic() |> 
        terra::project(y = rast(dem_file)) |> 
        terra::crop(y = rast(dem_file), mask = TRUE,
                       filename = target_file, overwrite = TRUE) 
    rm(naip_rast)
    gc()
    # } else {
    #     print("NAIP already processed")
    # }
    
    return(NULL)  
}
#####################################################################################

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

# Run with future_lapply
future_lapply(
    X = huc_list, 
    FUN = process_huc,
    future.packages = c("terra", "sf", "dplyr", "stringr"),
    future.seed = TRUE
)

# Run with non-parallel
# results <- lapply(
#     X = huc_list[1], 
#     FUN = process_huc
# )
# 
# 
# 
