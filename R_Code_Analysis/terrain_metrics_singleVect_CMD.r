#!/usr/bin/env Rscript

args = c("Data/DEMs/imgs/", 
         "Data/NY_HUCS/NY_HUCS_OpalescentRiver.gpkg", 
         "Data/DEMs/terrain_metrics/",
         21
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "DEM folder:", args[1], "\n", 
    "Path to a vector study area", args[2], "\n", 
    "Save folder:", args[3], "\n", 
    "Odd Integer:", args[4], "\n")

library(terra)
library(MultiscaleDTM)
#library(future)
suppressfuturesuppressPackageStartupMessages(library(tidyterra))



#A vrt for all DEM tiles across NYS to be extracted with a HUC/Study area 
dems <- terra::vrt(list.files(args[1],
                       pattern = ".img$",
                       full.names = TRUE))
vector_target <- vect(args[2]) |> project(crs(dems))

dems_target <- terra::crop(dems, vector_target)

writeRaster(dems_target, paste0(args[3], "test_dem.tif"),
            overwrite = TRUE)#This should write a raster as a DEM mosaic 

win <- c(as.numeric(args[4]), as.numeric(args[4]))

slp <- MultiscaleDTM::SlpAsp(dems_target, w = win, unit = "degrees", 
                                         include_scale = TRUE, metrics = "slope")
curv <- MultiscaleDTM::Qfit(dems_target, w = win,
                             include_scale = TRUE, metrics = c("meanc", "planc", "profc"))

writeRaster(c(slp,curv), filename = paste0(args[3], "_slp_curv_", args[4],".tif"),
            overwrite = TRUE)