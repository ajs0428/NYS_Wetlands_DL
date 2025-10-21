#!/usr/bin/env Rscript

args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         11,
         "Data/TerrainProcessed/HUC_DEMs/",
         "slp",
         "Data/TerrainProcessed/HUC_TerrainMetrics/"
)
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
library(future)
library(future.apply)
library(stringr)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.20,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
# cluster_target <- sf::st_read(args[1]) |> 
#     sf::st_transform(st_crs("EPSG:6347")) |>
#     dplyr::filter(cluster == args[2]) |> 
#     terra::vect() 

###############################################################################################
list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*.tif$"))) |> 
    stringr::str_subset(pattern = "wbt", negate = TRUE)

terrain_function <- function(dems_target, metric = args[4]){

    cluster_huc_name <- stringr::str_remove(basename(dems_target), ".tif")
    print(cluster_huc_name)
    
    tryCatch({
    if(stringr::str_detect(metric, "slp")){
        print("Slp")
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
            dems_target |>
                terra::rast() |>
                terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                               overwrite = TRUE, names = c("slope_5m", "aspect_5m", "TPI_5m", "TRI_5m"))
        } else {print(paste0(args[4], " 5m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                               overwrite = TRUE, names = c("slope_100m", "aspect_100m", "TPI_100m", "TRI_100m"))
        }else {print(paste0(args[4], " 100m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
            dems_target |>
                terra::rast() |>
                terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                               filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                               overwrite = TRUE, names = c("slope_500m", "aspect_500m", "TPI_500m", "TRI_500m"))
        } else {print(paste0(args[4], " 500m Metric files accounted for"))}
        
    } else if (stringr::str_detect(metric, "dmv")){
        print("DMV")
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE) |>
                # terra::wrap()
                writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                            overwrite = TRUE, names = c("dmv_5m"))
        } else {print(paste0(args[4], " 5m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE) |>
                # terra::wrap()
                writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                            overwrite = TRUE, names = c("dmv_100m"))
        }else {print(paste0(args[4], " 100m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = FALSE) |>
                # terra::wrap()
                writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                            overwrite = TRUE, names = c("dmv_500m"))
        }else {print(paste0(args[4], " 500m Metric files accounted for"))}
        
    } else if(stringr::str_detect(metric, "curv")){
        print("CURV")
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
            dems_target |> 
                terra::rast() |> 
                terra::aggregate(5, fun = "mean", na.rm = TRUE) |> 
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> 
                MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>

                writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                            overwrite = TRUE, names = c("meanc_5m", "planc_5m", "profc_5m"))
        } else {print(paste0(args[4], " 5m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
            dems_target |>
                terra::rast() |>
                terra::aggregate(100, fun = "mean", na.rm = TRUE) |>
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> 
                MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc"))|>

            writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                        overwrite = TRUE, names = c("meanc_100m", "planc_100m", "profc_100m"))
        }else {print(paste0(args[4], " 100m Metric files accounted for"))}
        if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
            dems_target |>
                terra::rast() |>
                terra::aggregate(500, fun = "mean", na.rm = TRUE) |>
                terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> 
                MultiscaleDTM::Qfit(w = c(3,3),include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>

            writeRaster(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                        overwrite = TRUE, names = c("meanc_500m", "planc_500m", "profc_500m"))
        } else {print(paste0(args[4], " 500m Metric files accounted for"))}
    } else {
        print("No terrain metric specified or not identified") 
    }
    }, error = function(msg){
        message(paste0("Error at: ", cluster_huc_name))
        return(NA)
    }
    )
}

# corenum <-  future::availableCores()
# options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
# 
# print(corenum)
# print(options()$future.globals.maxSize)

lapply(list_of_huc_dems, terrain_function)
# future_lapply(list_of_huc_dems, terrain_function, future.seed = TRUE)

# lapply(list_of_huc_dems, terrain_function, cluster_target)


## Fixing cluster_208_huc_041402011009_raster_predictors curvature 5m didnt' work
# "meanc_5m"       "planc_5m"       "profc_5m"

# bname <- basename(list_of_huc_dems[str_detect(list_of_huc_dems, "huc_041402011009")]) |> str_remove(".tif")
# 
# rf <- list_of_huc_dems[str_detect(list_of_huc_dems, "huc_041402011009")] 
# 
# rf |> 
#     terra::rast() |> 
#     terra::aggregate(5, fun = "mean", na.rm = TRUE) |> 
#     terra::resample(y = terra::rast(rf), method = "cubicspline") |> 
#     MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>
#     
#     writeRaster(paste0(args[5], bname, "_terrain_", args[4], "_5m.tif"),
#                 overwrite = TRUE, names = c("meanc_5m", "planc_5m", "profc_5m"))
