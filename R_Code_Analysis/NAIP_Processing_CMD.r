#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
    11,
    "Data/NAIP/NAIP_Processed/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
    "- Path to a file unprocessed NAIP files:", args[1], "\n",
    "- Cluster:", args[2], "\n"
))

###############################################################################################

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

#Index of all NAIP tiles
naip_index <- st_read("Data/NAIP/noaa_digital_coast_2017/tileindex_NY_NAIP_2017.shp", quiet = TRUE) |> 
    st_transform(st_crs("EPSG:6347"))

#Cluster of HUCs
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)

#Filter for NAIP tiles in Cluster
naip_int_cluster <- st_filter(naip_index, cluster_target, .predicate = st_intersects)
# plot(naip_int_cluster)

 
###############################################################################################

# This should take a list of all the NAIP rasters, merge them together in a HUC,
# crop to HUC boundaris, calculate indices, export and write to file
naip_processing_func <- function(naip_vector_tiles = naip_int_cluster){
    vi2 <- function(r, g, nir) {
        return(
            c(((nir - r) / (nir + r)), ((g-nir)/(g+nir)))
            )
    }
    target_crs <- cluster_crs

    
    for(i in seq_along(cluster_target$huc12)){
        
        target_file <- paste0(args[3], "cluster_", args[2], "_huc_", cluster_target$huc12[[i]], "_NAIP_metrics.tif")
        print(target_file)
        if(!file.exists(target_file)){
            print("no NAIP processed yet")
            
            naip_tiles_huc <- st_filter(naip_vector_tiles, cluster_target[1,])

            #re-paste the file path to rasters
            naip_int_cluster_rast_locs <- paste0("Data/NAIP/noaa_digital_coast_2017/", naip_tiles_huc$location)
            print(naip_int_cluster_rast_locs)
            n <- terra::vrt(naip_int_cluster_rast_locs) |>
                terra::project(target_crs, res = 1)
            np <- vi2(n[[1]], n[[2]], n[[4]])
            nall <- c(n, np)
            set.names(nall, c("r", "g", "b", "nir", "ndvi", "ndwi"))
            writeRaster(nall, 
                        filename = target_file,
                        overwrite = TRUE)
        } else {
            print("NAIP already processed")
        }
        
    }
    #return(naip_processed)
}

system.time({naip_processing_func()})

