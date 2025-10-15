#!/usr/bin/env Rscript

#################
# This script corrects the layer names and crs
#################

args = c(
         "Data/TerrainProcessed/HUC_Hydro//",
         22
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to the HUC Terrain Metrics:", args[1], "\n",
    "- cluster number:", args[2]
)

###############################################################################################

library(terra)
library(sf)
library(here)
library(foreach)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(tidyverse))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 8, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

r <- list.files(path = args[1], pattern = paste0("cluster_", args[2], ".*\\.tif"), full.names = TRUE)

rename_layers_func <- function(rast_list){
    for(i in 1:length(rast_list)){
        ri <- rast(rast_list[[i]])
        scale <- str_extract(rast_list[[i]], "\\d+m")
        tmpf <- tempfile(tmpdir = "Data/tmp", fileext = ".tif")
        
        if(!is.na(scale) && str_detect(scale, "10m") && !str_detect(names(ri)[[1]], "10m")){
            rn <- ri |> tidyterra::rename_with(~paste0(.x, "_10m"))
            names_correct = "no"
            } 
        else if(!is.na(scale) && str_detect(scale, "100m") && !str_detect(names(ri)[[1]], "100m")){
            rn <- ri |> tidyterra::rename_with(~paste0(.x, "_100m"))
            names_correct = "no"
            }
        else if(!is.na(scale) && str_detect(scale, "1000m") && !str_detect(names(ri)[[1]], "1000m")){
            rn <- ri |> tidyterra::rename_with(~paste0(.x, "_1000m"))
            names_correct = "no"
            }
        else {rn <- ri
             names_correct <- "yes"
             print("names correct")}

        if(crs(rn) != crs("EPSG:6347") && names_correct == "yes"){
           print("Not 6347 names correct")
           rnp <- terra::project(rn, "EPSG:6347", threads = TRUE)
           writeRaster(rnp, tmpf, overwrite = TRUE)
           rnp <- rast(tmpf)
           print(rnp)
           writeRaster(rnp, rast_list[[i]], overwrite = TRUE)
           file.remove(tmpf)
        } else if(crs(rn) == crs("EPSG:6347") & names_correct != "yes"){
            print("Is 6347, rewrite names")
            writeRaster(rn, tmpf, overwrite = TRUE)
            rn <- rast(tmpf)
            writeRaster(rn, rast_list[[i]], overwrite = TRUE)
            file.remove(tmpf)
        } else {print("Is 6347 and names correct")}
    }
}

system.time(
    rename_layers_func(r)    
)

###############################################################################################
