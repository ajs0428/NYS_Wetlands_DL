#!/usr/bin/env Rscript

args = c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
         11,
         "Data/TerrainProcessed/HUC_DEMs/",
         "slp",
         5,
         "Data/TerrainProcessed/HUC_TerrainMetrics/"
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Metric (slp, dmv, curv):", args[4], "\n", 
    "- Odd Integer:", args[5], "\n",
    "- Path to the Save folder", args[6], "\n",
    )
###############################################################################################

library(terra)
library(sf)
library(MultiscaleDTM)
library(foreach)
library(doParallel)
library(future)
library(future.apply)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
    # of interest.
    # cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1]) |> 
    dplyr::filter(cluster == args[2]) 


###############################################################################################
list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "_*\\*.tif$")))

terrain_function <- function(dem_fn, metric = args[4]){
    
    win <- c(as.numeric(args[5]), as.numeric(args[5]))
                
    cluster_huc_name <- stringr::str_remove(basename(dem_fn), ".tif")
    
    if(stringr::str_detect(metric, "slp")){
        dem_fn |> 
            terra::rast() |> 
            MultiscaleDTM::SlpAsp(w = win, unit = "degrees", 
                                  include_scale = TRUE, metrics = "slope",
                                  filename = paste0(args[6], cluster_huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                  overwrite = TRUE) 
    } else if (stringr::str_detect(metric, "dmv")){
        dem_fn |> 
            terra::rast() |> 
            MultiscaleDTM::DMV(w = win, stand = "none", # I think "none" so that NA won't be produced
                               include_scale = TRUE,
                                  filename = paste0(args[6], cluster_huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                  overwrite = TRUE) 
    } else if(stringr::str_detect(metric, "curv")){
        dem_fn |> 
            terra::rast() |> 
            MultiscaleDTM::Qfit(w = win,
                                include_scale = TRUE, metrics = c("meanc", "planc", "profc"),
                                filename = paste0(args[6], cluster_huc_name, huc_name, "_terrain_", args[4],"win",args[5], ".tif"),
                                overwrite = TRUE)
    } else {
        print("No terrain metric specified or not identified")
    }
    
    rm(c("cluster_huc_name", "dem_fn"))
}
    

terrain_function(list_of_huc_dems, cluster_target)



