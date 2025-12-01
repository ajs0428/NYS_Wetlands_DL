#!/usr/bin/env Rscript

#################
# This script corrects the layer names and crs
#################

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
    67,
    "Data/TerrainProcessed/HUC_TerrainMetrics/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to the HUC Terrain Metrics:", args[1], "\n",
    "- cluster number:", args[2],
    "- path to rasters:", args[3]
)

###############################################################################################

library(terra)
library(sf)
library(here)
library(future)
library(future.apply)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))


terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())
###############################################################################################
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)
###############################################################################################

rast_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
dem_list <- list.files(path = "Data/TerrainProcessed/HUC_DEMs/", pattern = paste0("cluster_", args[2], "_", ".+\\d+\\.tif$"), full.names = TRUE)

corr_layers_func <- function(i, cluster_target, rast_list, dem_list){
    cluster_huc_name <- cluster_target$huc12[[i]]
    message(cluster_huc_name)

    r_filename <- rast_list[str_detect(rast_list, cluster_huc_name)]
    dem_filename <- dem_list[str_detect(dem_list, cluster_huc_name)]
    ri <- rast(r_filename)
    di <- rast(dem_filename)
    r_ext <- ext(ri)
    d_ext <- ext(di)
    tmpf <- tempfile(tmpdir = "Data/tmp", fileext = ".tif")

    if(crs(ri) != crs(di) & r_ext != d_ext){
       message("Not 6347, different extent")
       rip <- terra::project(ri, crs(di), threads = TRUE, res = 1,
                             mask = TRUE, method = "bilinear")
       writeRaster(rip, tmpf, overwrite = TRUE)
       rip <- rast(tmpf)
       writeRaster(rip, rast_list[[i]], overwrite = TRUE)
       file.remove(tmpf)
       return(NULL)
    } else if(crs(ri) != crs(di) & r_ext == d_ext){
        message("crs different, but ext equal")
        rip <- terra::project(ri, crs(di), threads = TRUE, res = 1,
                              mask = TRUE, method = "bilinear")
        writeRaster(rip, tmpf, overwrite = TRUE)
        rip <- rast(tmpf)
        writeRaster(rip, rast_list[[i]], overwrite = TRUE)
        file.remove(tmpf)
        return(NULL)
    } else if(crs(ri) == crs(di) & r_ext != d_ext){
        message("extent different, resampling")
        rip <- terra::resample(ri, di, method = "bilinear")
        writeRaster(rip, tmpf, overwrite = TRUE)
        rip <- rast(tmpf)
        writeRaster(rip, rast_list[[i]], overwrite = TRUE)
        file.remove(tmpf)
        return(NULL)
    } else {
        message("extent and CRS fine")
        return(NULL)
        }
    
}

plan(multicore, workers = 4)
options(future.globals.maxSize= 32 * 1e9)

future_lapply(
    seq_along(cluster_target$huc12),
    corr_layers_func,
    cluster_target = cluster_target,
    rast_list = rast_list,
    dem_list = dem_list,
    future.seed = TRUE
)

###############################################################################################
# lapply(
#     seq_along(cluster_target$huc12),
#     corr_layers_func,
#     cluster_target = cluster_target,
#     rast_list = rast_list,
#     dem_list = dem_list
# )
