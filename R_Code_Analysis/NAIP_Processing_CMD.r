#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
    120,
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

vi2 <- function(r, g, nir) {
    return(
        c(((nir - r) / (nir + r)), ((g-nir)/(g+nir)))
        )
}

process_huc <- function(i, cluster_target, naip_int_cluster, args) {
    target_file <- paste0(args[3], "cluster_", args[2], "_huc_", cluster_target$huc12[[i]], "_NAIP_metrics.tif")
    dem_filename <- paste0("Data/TerrainProcessed/HUC_DEMs", "/cluster_", args[2], "_huc_", cluster_target$huc12[[i]], ".tif")
    
    # uncomment the if statement with file.exists to ignore files already created
   # if(!file.exists(target_file)){
    print("no NAIP processed yet")
    
    naip_tiles_huc <- st_filter(naip_int_cluster, cluster_target[i,])
    # print(naip_tiles_huc)
    #re-paste the file path to rasters
    naip_int_cluster_rast_locs <- paste0("Data/NAIP/noaa_digital_coast_2017/", naip_tiles_huc$location)
    print(naip_int_cluster_rast_locs)
    n <- terra::sprc(naip_int_cluster_rast_locs) |>
        terra::mosaic(fun = "max") |>
        terra::project("EPSG:6347", res = 1) |>
        terra::crop(cluster_target[i,] |> vect(), mask = TRUE) |>
        resample(y = rast(dem_filename))
    np <- vi2(n[[1]], n[[2]], n[[4]])
    nall <- c(n, np)
    set.names(nall, c("r", "g", "b", "nir", "ndvi", "ndwi"))
    
    writeRaster(nall,
                filename = target_file,
                overwrite = TRUE)
    rm(n)
    rm(np)
    rm(nall)
    gc()
    # } else {
    #     print("NAIP already processed")
    # }
    
    return(NULL)  # or return something meaningful if needed
}

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

# Run with future_lapply
results <- future_lapply(
    seq_along(cluster_target$huc12),
    FUN = process_huc,
    cluster_target = cluster_target,
    naip_int_cluster = naip_int_cluster,
    args = args,
    # future.packages = c("terra", "sf", "tidyverse", "tidyterra"),
    future.seed = TRUE
)

# Run with non-parallel
# results <- lapply(
#     seq_along(cluster_target$huc12)[1],
#     FUN = process_huc,
#     cluster_target = cluster_target,
#     naip_int_cluster = naip_int_cluster,
#     args = args
# )


