#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
    208,
    "Data/NAIP/NAIP_Processed/",
    "Data/NAIP/NAIP_HUC_Merged/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area:", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to processed NAIP files:", args[3], "\n",
    "- Path to folder for HUC NAIP merged:", args[4], "\n"
)

###############################################################################################

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
library(foreach)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

cluster_target <- sf::st_read(args[1]) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() #### |> terra::wrap()

split_spatvector_to_list <- function(spatvector) {
    n_polygons <- nrow(spatvector)
    polygon_list <- vector("list", n_polygons)
    
    for (i in 1:n_polygons) {
        polygon_list[[i]] <- spatvector[i, ]
    }
    
    return(polygon_list)
}

huc_list <- split_spatvector_to_list(cluster_target)

naip_processed_list <- list.files(args[3], full.names = TRUE, pattern = ".tif") |> lapply(rast)

###############################################################################################

naip_extract_merge <- function(raster_list, vector_list) {

    raster_extents <- lapply(raster_list, ext)
    vector_extents <- lapply(vector_list, ext)
    
    for(i in seq_along(vector_extents)){
        huc_name <- vector_list[[i]][["huc12"]][[1]]
        if(!file.exists(paste0(args[4], "NAIP_Metrics_cluster_",args[2],"_HUC_", huc_name, ".tif"))){
            print("New NAIP for HUC")
            overlap <- which(sapply(raster_extents, 
                                    function(re) {
                                        relate(re, vector_extents[[i]], "intersects")
                                    }))
            print(overlap)
            raster_list[overlap] |>
                terra::sprc() |>
                terra::merge() |>
                terra::crop(y = vector_list[[i]], mask = TRUE,
                            filename = paste0(args[4], "NAIP_Metrics_cluster_",args[2],"_HUC_", huc_name, ".tif"))
            
        } else {
            print("NAIP Already Exists")
        }
        
    }
    
}


naip_extract_merge(naip_processed_list, huc_list)

