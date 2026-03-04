#!/usr/bin/env Rscript

args = c(
    "Data/CHMs/HUC_CHMs/",
    "Data/CHMs/HUC_CHMvar/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
     "- Path to processed CHM files:", args[1], "\n",
     "- Path to save CHM variance files:", args[2], "\n"
))

###############################################################################################
library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())
###############################################################################################

chm_files <- grep(list.files(args[1], full.names = TRUE), pattern = "wbt|NA", invert=TRUE, value=TRUE) 

###############################################################################################

variance_calc <- function(chm){

    chm_filename <- paste0(args[2], str_remove(basename(chm), ".tif"), "_var.tif")
    
    if(!file.exists(chm_filename)){
        chm_rast <- rast(chm)
        chm_rast_var <- c(
            terra::focal(chm_rast, w = 21, fun = "min", 
                         na.policy="all", na.rm=TRUE, expand=FALSE, fillvalue=NA,
                         names = paste0("CHM_min_", "21")),
            terra::focal(chm_rast, w = 21, fun = "max", 
                         na.policy="all", na.rm=TRUE, expand=FALSE, fillvalue=NA,
                         names = paste0("CHM_max_", "21")),
            terra::focal(chm_rast, w = 21, fun = "sd", 
                         na.policy="all", na.rm=TRUE, expand=FALSE, fillvalue=NA,
                         names = paste0("CHM_sd_", "21"))
        )
        writeRaster(chm_rast_var, filename = chm_filename, overwrite = TRUE)
    } else {
        message(paste0("file already exists skipping", chm_filename))
    }
    rm(chm_rast)
    rm(chm_rast_var)
    gc()
}

#### Single core run
lapply(chm_files[1:2],  variance_calc)



if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

future_lapply(gee_files, match_align_project, future.seed = TRUE)