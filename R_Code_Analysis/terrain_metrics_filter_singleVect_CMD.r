#!/usr/bin/env Rscript

args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         208,
         "Data/TerrainProcessed/HUC_DEMs/",
         "dmv",
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
library(stringr)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.20,# Use only 10% of memory for program
             memmax = 16, #max memory is 8Gb
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
            .export = "args",
            .errorhandling = "remove") %dopar% {
                
        cluster_huc_name <- stringr::str_remove(basename(list_of_dem_rasts[[i]]), ".tif")
        dems_target <- list_of_dem_rasts[[i]]
        
        if(stringr::str_detect(metric, "slp")){
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
                dems_target |>
                    terra::rast() |>
                    terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                                   filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                                   overwrite = TRUE, names = c("slope_5m", "aspect_5m", "TPI_5m", "TRI_5m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
                dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                                   filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                                   overwrite = TRUE, names = c("slope_100m", "aspect_100m", "TPI_100m", "TRI_100m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
                dems_target |>
                    terra::rast() |>
                    terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
                                   filename = paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                                   overwrite = TRUE, names = c("slope_500m", "aspect_500m", "TPI_500m", "TRI_500m"))
            } else {outfile("Metric files accounted for")}
            
        } else if (stringr::str_detect(metric, "dmv")){
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
                d5 <-dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                       include_scale = FALSE) |>
                    terra::wrap()
                writeRaster(terra::unwrap(d5) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                            overwrite = TRUE, names = c("dmv_5m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
                d100 <- dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                       include_scale = FALSE) |>
                    terra::wrap()
                writeRaster(terra::unwrap(d100) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                            overwrite = TRUE, names = c("dmv_100m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
                d500 <- dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
                                       include_scale = FALSE) |>
                    terra::wrap()
                writeRaster(terra::unwrap(d500) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                            overwrite = TRUE, names = c("dmv_500m"))
            }
            
        } else if(stringr::str_detect(metric, "curv")){
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"))){
                c5 <-dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>
                    terra::wrap()
                writeRaster(terra::unwrap(c5) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif"),
                            overwrite = TRUE, c("meanc_5m", "planc_5m", "profc_5m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"))){
                c100 <- dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc"))|>
                    terra::wrap()
                writeRaster(terra::unwrap(c100) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif"),
                            overwrite = TRUE, c("meanc_100m", "planc_100m", "profc_100m"))
            }
            if(!file.exists(paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"))){
                c500 <- dems_target |> 
                    terra::rast() |> 
                    terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
                    terra::resample(y = terra::rast(dems_target), method = "cubicspline") |> # resample to original resolution
                    MultiscaleDTM::Qfit(w = c(3,3),include_scale = TRUE, metrics = c("meanc", "planc", "profc"))|>
                    terra::wrap()
                writeRaster(terra::unwrap(c500) ,paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif"),
                            overwrite = TRUE, c("meanc_500m", "planc_500m", "profc_500m"))
            } else {outfile("Metric files accounted for")}

        } else {
            print("No terrain metric specified or not identified")
        }
        
        #rm(c("cluster_huc_name", "dems_target"))
    }
    stopCluster(cl)
}
    

terrain_function(list_of_huc_dems, cluster_target)
