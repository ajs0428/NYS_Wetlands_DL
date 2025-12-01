#!/usr/bin/env Rscript

args = c(
         "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg", #1
         208, #2
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
library(foreach)
library(doParallel)
library(collapse)
library(future)
library(future.apply)
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(tidyverse))

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 

###############################################################################################
ny_pts <- list.files("Data/Training_Data", 
                     pattern = paste0("cluster_", args[2], "(_.*)?_training_pts.gpkg$"), 
                     full.names = TRUE) |> 
    lapply(sf::st_read, quiet = TRUE) |> 
    lapply(sf::st_transform, st_crs("EPSG:6347")) |> 
    dplyr::bind_rows()

# ny_pts <- vect("Data/Training_Data/Northern_Allegheny_Plateau_training_pts.gpkg") |> 
#     terra::project("EPSG:6347")

pts_list <- list()
for(i in 1:nrow(cluster_target["huc12"])){
    huc_name <- cluster_target[i,]["huc12"][[1]]
    huc <- cluster_target |> dplyr::filter(huc12 == huc_name)
    pts_list[[i]] <- sf::st_filter(ny_pts, huc)
}
names(pts_list) <- as.vector(cluster_target["huc12"][[1]])
print(names(pts_list))
###############################################################################################
#pts_list[1:2] 
dem_list <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = paste0("cluster_", args[2], "_", ".*\\d+.tif"), full.names = TRUE)
terr_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], "_", ".*\\m.tif"), full.names = TRUE) %>% 
    .[!str_detect(., "1000m")]
hydro_list <- list.files(path = args[4], pattern = paste0("cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
naip_list <- list.files(path = args[5], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
chm_list <- list.files(path = args[6], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)

###############################################################################################
raster_stack_extract <- function(terr_list, hydro_list, naip_list, chm_list, pts_list){

    # Extract function for parallel execution
    extract_single_huc <- function(i) {
        huc_name <- names(pts_list[i])
        message(huc_name)  # message() works better with parallel processing
        pts <- terra::vect(pts_list[[i]])
        
        pts_extracted <- terra::extract(y = pts, 
                                        x = setNames(rast(dem_list[str_detect(dem_list, huc_name)]), 
                                                     "DEM"),
                                        bind = TRUE) |>
            terra::extract(x = rast(terr_list[str_detect(terr_list, huc_name)]),
                           bind = TRUE
                           ) |> 
            terra::extract(x = rast(hydro_list[str_detect(hydro_list, huc_name)]), 
                           bind = TRUE) |> 
            terra::extract(x = rast(naip_list[str_detect(naip_list, huc_name)]),
                           bind = TRUE
                           ) |>
            terra::extract(x = rast(chm_list[str_detect(chm_list, huc_name)]),
                           bind = TRUE
            ) |>
            tidyterra::mutate(huc = huc_name,
                              cluster = args[2])
        
        writeVector(pts_extracted, filename = paste0("Data/Training_Data/HUC_Extracted_Training_Data/", "cluster_", args[2], "_huc_", huc_name, "ext_train_pts.gpkg"),
                    overwrite =TRUE)

        # Clean up raster from memory
        gc(verbose = FALSE)
        
        return(pts_extracted)
    }
    
    # Parallel execution
    pts_extract_list <- future_lapply(seq_along(pts_list), 
                                      extract_single_huc,
                                      future.seed = TRUE)
    
    # Combine results and write immediately
    pts_extract <- vect(pts_extract_list)
    
    writeVector(pts_extract, 
                filename = paste0(args[7], "cluster_", args[2], "_extracted_training_pts.gpkg"),
                overwrite = TRUE)
    
    # Clean up
    rm(pts_extract_list)
    gc(verbose = FALSE)
    
    return(invisible(NULL))
}


###############################################################################################

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 32 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

system.time({raster_stack_extract(
    terr_list = terr_list,
    hydro_list = hydro_list, 
    naip_list = naip_list,
    chm_list = chm_list,
    pts_list = pts_list
)})
