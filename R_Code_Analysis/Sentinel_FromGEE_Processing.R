#!/usr/bin/env Rscript

args = c(
    "Data/TerrainProcessed/HUC_DEMs/",
    "Data/Satellite/GEE_Download_NY_HUC_Sentinel_Indices/ny_huc_indices",
    "Data/Satellite/HUC_Processed_NY_Sentinel_Indices/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
     "- Path to processed DEM files:", args[1], "\n",
     "- Path to processed GEE Downloaded Sentinel files:", args[2], "\n",
     "- Path to save processed Sentinel files:", args[3], "\n"
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

dem_files <- grep(list.files(args[1], full.names = TRUE), pattern = "wbt|NA", invert=TRUE, value=TRUE) 

dem_hucs <- str_extract(dem_files, "(?<=huc_)\\d+(?=\\.tif)")

gee_files <- list.files(args[2], full.names = TRUE, pattern = ".tif")

gee_hucs <- str_extract(gee_files, "(?<=/)\\d+(?=_)")

dem_files_w_gee <- dem_files[dem_hucs %in% gee_hucs]

###############################################################################################

match_align_project <- function(single_gee_path){

    single_gee_basename <- basename(single_gee_path)
    
    single_gee_huc_num <- str_extract(single_gee_basename, "^\\d+")
    
    single_dem_file <- dem_files[str_detect(dem_files, single_gee_huc_num)]
    single_dem_filename <- str_remove(basename(single_dem_file), ".tif")
    gee_sentinel_filename <- paste0(args[3], single_dem_filename, "_sentinel_indices.tif")
    
    if(!file.exists(gee_sentinel_filename)){
        dem_rast <- rast(single_dem_file)
        gee_rast_process <- rast(single_gee_path) |> 
            terra::project(y = dem_rast, method = "cubicspline", mask = TRUE,
                           filename = gee_sentinel_filename)
        
        tryCatch({
            c(dem_rast, gee_rast_process)
        }, error = function(e){
            message("Error on stacking?: ", e$message)
            return(NA)
        })
    } else {
        message(paste0("file already exists skipping", gee_sentinel_filename))
    }
    rm(dem_rast)
    rm(gee_rast_process)
    gc()
}

# Single core run
# lapply(gee_files[1:2],  match_align_project)



if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

future_lapply(gee_files, match_align_project, future.seed = TRUE)