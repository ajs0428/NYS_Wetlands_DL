#!/usr/bin/env Rscript

# args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
#          208,
#          "Data/TerrainProcessed/HUC_DEMs/",
#          "slp",
#          "Data/TerrainProcessed/HUC_TerrainMetrics/"
#          )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Metric (slp, dmv, curv):", args[4], "\n", 
    "- Path to the Save folder", args[5], "\n"
    )
###############################################################################################

library(terra)
library(sf)
library(MultiscaleDTM)
library(foreach)
library(stringr)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 8, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
    # of interest.
    # cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1]) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() |> 
    terra::wrap()


###############################################################################################
list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "*\\*.tif$"))) |> 
   stringr::str_subset(pattern = "wbt", negate = TRUE)

terrain_function <- function(list_of_dem_rasts, cluster, metric = args[4]){
    cl <- makeCluster(8, outfile = "/ibstorage/anthony/NYS_Wetlands_GHG/Shell_Scripts/terrain_cl.log")
    registerDoParallel(cl)
    
    foreach(i = seq_along(list_of_dem_rasts), 
            .packages = c("terra", "tidyterra"),
            .export = "args") %dopar% {
                
        cluster_huc_name <- stringr::str_remove(basename(list_of_dem_rasts[[i]]), ".tif")
        dems_target <- list_of_dem_rasts[[i]]
        
        if(stringr::str_detect(metric, "slp")){
            dems_target |>
                terra::rast() |>
                terra::aggregate(10, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_10m.tif"),
                               overwrite = TRUE)
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                               overwrite = TRUE)
            dems_target |>
                terra::rast() |>
                terra::aggregate(1000, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_1000m.tif"),
                               overwrite = TRUE)
        } else if (stringr::str_detect(metric, "dmv")){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(10, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE,
                                      filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_10m.tif"),
                                      overwrite = TRUE) 
            print("DMV 10")
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE,
                                   filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                                   overwrite = TRUE) 
            print("DMV 100")
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(1000, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE,
                                   filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_1000m.tif"),
                                   overwrite = TRUE) 
            print("DMV 1000")
        } else if(stringr::str_detect(metric, "curv")){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(10, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::Qfit(w = c(3,3),
                                    include_scale = TRUE, metrics = c("meanc", "planc", "profc"),
                                    filename = paste0(args[5], cluster_huc_name, huc_name, "_terrain_", args[4], "_10m.tif"),
                                    overwrite = TRUE)
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::Qfit(w = c(3,3),
                                    include_scale = TRUE, metrics = c("meanc", "planc", "profc"),
                                    filename = paste0(args[5], cluster_huc_name, huc_name, "_terrain_", args[4], "_100m.tif"),
                                    overwrite = TRUE)
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(1000, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::Qfit(w = c(3,3),
                                    include_scale = TRUE, metrics = c("meanc", "planc", "profc"),
                                    filename = paste0(args[5], cluster_huc_name, huc_name, "_terrain_", args[4], "_1000m.tif"),
                                    overwrite = TRUE)
        } else {
            print("No terrain metric specified or not identified")
        }
        
        #rm(c("cluster_huc_name", "dems_target"))
    }
    stopCluster(cl)
}
    

terrain_function(list_of_huc_dems, cluster_target)

# 
# 
# rt <- rast("Data/TerrainProcessed/HUC_DEMs/cluster_208_huc_041402011002.tif")
# rts <- terra::terrain(rt, v = "slope")
# r10 <- rt |> 
#     terra::aggregate(fact = 10, fun = "mean", na.rm = TRUE) |> 
#     terra::resample(y = rt, method = "cubicspline") 
# r100 <- rt |> 
#     terra::aggregate(100, fun = "mean", na.rm = TRUE) |> 
#     terra::resample(y = rt, method = "cubicspline") 
# r1000 <- rt |> 
#     terra::aggregate(1000, fun = "mean", na.rm = TRUE) |> 
#     terra::resample(y = rt, method = "cubicspline") 
# sr10 <- terra::terrain(r10, v = "slope") 
# sr100 <- terra::terrain(r100, v = "slope") 
# sr1000 <- terra::terrain(r1000, v = "slope") 
# 
# 
# plot(c(sr10, sr100, sr1000), nc = 3)
# plot(c(rt, r10, r100), nc = 3)