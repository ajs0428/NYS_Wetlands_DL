#!/usr/bin/env Rscript

cat("args should be:", "\n",
    "1) path to DEM folder", "\n", 
    "2) path to save folder", "\n",
    "3) path to HUC watershed vector file \n",
    "4) an odd integer for the window size and scale", "\n",
    "5) the number of cores for the computer to use", "\n")

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here
# args <- c("Data/DEMs/imgs/test/",
#           "Data/TerrainProcessed/", 
#           "Data/NY_HUCS/NY_HUC12_subset.gpkg", 
#           "5", 
#           "8")
cat("these are the arguments: \n", 
    "DEM folder:", args[1], "\n", 
    "Save folder:", args[2], "\n", 
    "HUC folder:", args[3], "\n", 
    "Odd Integer:", args[4], "\n", 
    "Number of cores:", args[5], "\n")


stopifnot("The number of arguments is less than 4" = length(args) >= 4)

options(rgl.useNULL = TRUE) # prevents an rgl warning
library(terra)
library(MultiscaleDTM)
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)

corenum <- as.numeric(args[5]) # fifth argument is cores
stopifnot("Too many cores specified" = corenum <= future::availableCores()[[1]])
plan(multisession, workers = corenum) # number of cores is decided by availableCores() and should work with Slurm scheduler
                        # this should probably be an argument for bash

path_var = args[2] #the second argument should be a file path to save to
win_dim = as.numeric(args[4]) # the fourth argument should be an odd integer for the window size and scale

stopifnot("The dimensions of the window are even" = win_dim%%2 != 0)

dems <- vrt(list.files(args[1],
                        pattern = ".img$",
                        full.names = TRUE))
#print(dems)
hucs <- vect(args[3])  |> # third argument is HUC folder
    terra::project(crs(dems))

huc_names <- as.list(hucs[["huc12"]][[1]])


terrain_calc_func %<-% function(hn, window = c(win_dim,win_dim) ){
    #print(window)
    print(hn)
    huc <- tidyterra::filter(hucs, huc12 == hn)
    print(huc)
    crp <- terra::crop(dems, huc)
    print(crp)
    mos <- crp #terra::mosaic(crp)
    
    writeRaster(mos, filename = paste0(path_var, "/", hn, "_DEM_mosaic_", win_dim, ".tif"),
                overwrite = T)
    
    # name <- tools::file_path_sans_ext(basename(dem_path))

    s <- MultiscaleDTM::Pfit(mos,
                             w = window,
                             metrics = "pslope",
                             include_scale = TRUE,
                             na.rm = TRUE
                             )
    writeRaster(s, filename = paste0(path_var, "/", hn, "_slope_", win_dim, ".tif"),
                overwrite = T)
    t <- MultiscaleDTM::DMV(mos,
                            w = window,
                            shape = "rectangle",
                            stand = "range",
                            include_scale = TRUE,
                            na.rm = TRUE
    )
    writeRaster(t, filename = paste0(path_var, "/", hn, "_DMV_", win_dim, ".tif"),
                overwrite = T)
    q <- MultiscaleDTM::Qfit(mos,
                             w = window,
                             na.rm = TRUE,
                             include_scale = TRUE,
                             metrics = c("meanc"))
    writeRaster(q, filename = paste0(path_var, "/", hn, "_curve_",  win_dim, ".tif"),
                overwrite = T)

   
}

system.time({future_lapply(huc_names, terrain_calc_func, future.seed = T)})
#system.time({huc_names, terrain_calc_func)})

