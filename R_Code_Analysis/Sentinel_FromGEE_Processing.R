#!/usr/bin/env Rscript

args = c(
    "Data/TerrainProcessed/HUC_DEMs/",
    "Data/Satellite/GEE_Download_NY_HUC_Sentinel_Indices/ny_huc_indices",
    "Data/Satellite/HUC_Processed_NY_Sentinel_Indices/",
    208
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(message("these are the arguments: \n", 
     "- Path to processed DEM files: ", args[1], "\n",
     "- Path to processed GEE Downloaded Sentinel files: ", args[2], "\n",
     "- Path to save processed Sentinel files: ", args[3], "\n",
     "- Cluster number ", args[4], "\n"
))

###############################################################################################
library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_DL/Data/tmp")
print(tempdir())
###############################################################################################

dem_files <- grep(list.files(args[1], full.names = TRUE), pattern = "wbt|NA", invert=TRUE, value=TRUE) 
dem_files_clust <- dem_files[grepl(dem_files, pattern = paste0("cluster_", args[4], "_"))]
dem_hucs <- str_extract(dem_files_clust, "(?<=huc_)\\d+(?=\\.tif)")
dem_hucs_pattern <- str_c(dem_hucs, collapse = "|")

gee_files <- list.files(args[2], full.names = TRUE, pattern = ".tif")
gee_files_clust <- gee_files[str_detect(gee_files, dem_hucs_pattern)]
gee_hucs <- str_extract(gee_files_clust, "(?<=/)\\d+(?=_)")

dem_files_w_gee <- dem_files_clust[dem_hucs %in% gee_hucs]

###############################################################################################

match_align_project <- function(single_gee_path){

    single_gee_basename <- basename(single_gee_path)
    message("GEE basename: ", single_gee_basename)
    single_gee_huc_num <- str_extract(single_gee_basename, "^\\d+")
    message("GEE huc: ",single_gee_huc_num)
    single_dem_file <- dem_files[str_detect(dem_files, single_gee_huc_num)]
    single_dem_filename <- str_remove(basename(single_dem_file), ".tif")
    message("DEM filename: ",single_dem_filename)
    gee_sentinel_filename <- paste0(args[3], single_dem_filename, "_sentinel_indices.tif")
    message("GEE filename: ",gee_sentinel_filename)
    # if(file.exists(gee_sentinel_filename)){
        dem_rast <- rast(single_dem_file)
        gee_rast_process <- rast(single_gee_path) |>
            terra::project(y = dem_rast, method = "cubicspline", mask = TRUE,
                           filename = gee_sentinel_filename, overwrite = TRUE)

        tryCatch({
            c(dem_rast, gee_rast_process)
        }, error = function(e){
            message("Error on stacking?: ", e$message)
            return(NA)
        })
    # } else {
    #     message(paste0("file already exists skipping", gee_sentinel_filename))
    # }
    rm(dem_rast)
    rm(gee_rast_process)
    gc()
}

# Single core run
# lapply(gee_files_clust,  match_align_project)



if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

future_lapply(gee_files_clust, match_align_project, future.seed = TRUE, future.globals = TRUE)