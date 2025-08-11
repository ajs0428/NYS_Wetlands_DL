#!/usr/bin/env Rscript

# args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
#          208,
#          "Data/TerrainProcessed/",
#          "slp",
#          5
#          )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Metric (slp, dmv, curv):", args[4], "\n", 
    "- Odd Integer:", args[5], "\n"
    )
###############################################################################################

library(terra)
library(sf)
library(MultiscaleDTM)
library(foreach)
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
list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "*\\*.tif$")))

terrain_function <- function(list_of_dem_rasts, cluster, metric = args[4]){
    cl <- makeCluster(4)
    registerDoParallel(cl)
    
    win <- c(as.numeric(args[5]), as.numeric(args[5]))
    
    foreach(i = seq_along(list_of_dem_rasts), 
            .packages = c("terra", "tidyterra"),
            .export = "args") %dopar% {
                
        cluster_huc_name <- stringr::str_remove(basename(list_of_dem_rasts[i]), ".tif")
        dems_target <- list_of_dem_rasts[[i]]
        
        if(stringr::str_detect(metric, "slp")){
            dems_target |> 
                terra::rast() |> 
                MultiscaleDTM::SlpAsp(w = win, unit = "degrees", 
                                      include_scale = TRUE, metrics = "slope",
                                      filename = paste0(args[3], cluster_huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                      overwrite = TRUE) 
        } else if (stringr::str_detect(metric, "dmv")){
            dems_target |> 
                terra::rast() |> 
                MultiscaleDTM::DMV(w = win, stand = "none", # I think "none" so that NA won't be produced
                                   include_scale = TRUE,
                                      filename = paste0(args[3], cluster_huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                      overwrite = TRUE) 
        } else if(stringr::str_detect(metric, "curv")){
            dems_target |> 
                terra::rast() |> 
                MultiscaleDTM::Qfit(w = win,
                                    include_scale = TRUE, metrics = c("meanc", "planc", "profc"),
                                    filename = paste0(args[3], cluster_huc_name, huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                    overwrite = TRUE)
        } else {
            print("No terrain metric specified or not identified")
        }
    }
    stopCluster(cl)
}
    

terrain_function(list_of_huc_dems, cluster_target)



