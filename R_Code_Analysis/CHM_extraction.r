#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
    11,
    "Data/CHMs/AWS"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(message("these are the arguments: \n", 
     "- Path to a file unprocessed CHM files:", args[1], "\n",
     "- Path to processed CHM files:", args[2], "\n"
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

cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)
###############################################################################################

# This is all the CHM file names 
# chms_file_list <- list.files("Data/CHMs/AWS/",
#                              pattern = ".tif",
#                              full.names = TRUE,
#                              recursive = TRUE,
#                              include.dirs = FALSE)
# chms_file_list_limit <- chms_file_list[sapply(chms_file_list, file.size) > 100E3] # a lot of empty tiles
# chms_file_list_limit_base <- sub(".*AWS//", "", chms_file_list_limit)
# 
# saveRDS(chms_file_list, "Data/CHMs/chms_file_list.rds")
# saveRDS(chms_file_list_limit, "Data/CHMs/chms_file_list_limit.rds")
# saveRDS(chms_file_list_limit_base, "Data/CHMs/chms_file_list_limit_base.rds")

chms_file_list <- readRDS("Data/CHMs/chms_file_list.rds")
chms_file_list_limit <- readRDS("Data/CHMs/chms_file_list_limit.rds")
chms_file_list_limit_base <- readRDS("Data/CHMs/chms_file_list_limit_base.rds")

print(paste0("this is the total list of chm indexes: ", length(chms_file_list)[[1]]))
print(paste0("this is the limited list of chm indexes: ", length(chms_file_list_limit)[[1]]))
print(paste0("this is the limited list of chm basenames: ", length(chms_file_list_limit_base)[[1]]))

chms_gpkg_list <- list.files(args[3],
                             pattern = ".gpkg$",
                             full.names = TRUE,
                             recursive = FALSE)
print(chms_gpkg_list)
###############################################################################################

# This should make a list of all the CHM indexes that cross the area of the target cluster

chm_ind_fun <- function(chms_gpkg_fn){
    message("Processing file ", chms_gpkg_fn)
    
    features <- st_read(chms_gpkg_fn, quiet = TRUE)
    features_locs_base <- sub(".*AWS//", "", features$location)
    features_filter <- features[(features_locs_base %in% chms_file_list_limit_base),]
    # Transform to common CRS
    if (!st_crs(features_filter) == cluster_crs) {
        cat("  Transforming features to match polygon CRS...\n")
        features_filter <- st_transform(features_filter, cluster_crs)
    } else {
        features_filter <- features_filter
    }
    
    features_in_cluster <- st_filter(features_filter, cluster_target, .predicate = st_intersects) 
    return(features_in_cluster)
    # rm(features)
    # rm(features_in_cluster)
}

all_crossing_features <- lapply(chms_gpkg_list, chm_ind_fun)

final_crossing_features <- dplyr::bind_rows(all_crossing_features)

# final_crossing_features_rasts <- paste0(args[3], "/", final_crossing_features$location)
# final_crossing_features_vrt <- vrt(final_crossing_features_rasts) |> 
#                                 terra::project("EPSG:6347")
# ###############################################################################################
# 
# cluster_chm_extract <- terra::crop(final_crossing_features_vrt, cluster_target |> vect(),
#                                    mask = TRUE)
###############################################################################################

#### Simple for loop 
# for(i in seq_along(cluster_target$huc12)){
#     cluster_huc_name <- cluster_target$huc12[[i]]
#     print(cluster_huc_name)
#     
#     huc_chms <- st_filter(final_crossing_features, cluster_target[i,], .predicate = st_intersects) 
#     huc_rasts <- paste0(args[3], "/",  huc_chms$location)
#     
#     chm_filename <- paste0("Data/CHMs/HUC_CHMs", "/cluster_", args[2], "_huc_", cluster_huc_name, "_CHM.tif")
#     
#     huc_chm_vrt <- terra::vrt(huc_rasts) |> 
#         terra::project("EPSG:6347", res = 1) |> 
#         terra::crop(y = cluster_target[i,], mask = TRUE, 
#                     filename = chm_filename)
# }

###############################################################################################

#### Parallel setup for future_lapply or future_sapply
target_hucs <- cluster_target$huc12

process_huc <- function(cluster_huc_name) {
    chm_filename <- paste0("Data/CHMs/HUC_CHMs", "/cluster_", args[2], "_huc_", cluster_huc_name, "_CHM.tif")
    dem_filename <- paste0("Data/TerrainProcessed/HUC_DEMs", "/cluster_", args[2], "_huc_", cluster_huc_name, ".tif")
    message(chm_filename)
    
    dem_rast <- rast(dem_filename)
    is_not_empty <- function(r) {
        !all(is.na(values(r)))
    }

    if(!file.exists(chm_filename) & file.exists(dem_filename)){
        huc_target <- cluster_target[cluster_target$huc12 == cluster_huc_name, ]
        huc_chms <- st_filter(final_crossing_features, huc_target, .predicate = st_intersects)
        huc_file_locs <- paste0(args[3], "/",  huc_chms$location)
        huc_rasts <- lapply(huc_file_locs, rast)
        huc_file_locs_not_empty <- huc_file_locs[sapply(huc_rasts, is_not_empty)]

        huc_chm_merge <- terra::sprc(huc_file_locs_not_empty) |>
            terra::mosaic(fun = "max") |>
            terra::project("EPSG:6347", res = 1) |>
            terra::crop(y = huc_target, mask = TRUE) |>
            resample(y = dem_rast) |> 
            tidyterra::rename("CHM" = 1)
        terra::mask(huc_chm_merge, (!is.na(dem_rast) & is.na(huc_chm_merge)),
                    maskvalues=TRUE, updatevalue = 0, filename = chm_filename,
                    overwrite = TRUE)
        
    # } else if(file.exists(chm_filename) & file.exists(dem_filename)){
    #     print(paste0("File already exists: ", chm_filename))
    #     return(chm_filename)
    } else if(file.exists(chm_filename) & !file.exists(dem_filename)){
        print(paste0("DEM does not exist?: ", dem_filename))
        return(dem_filename)
    } else {
        print("Error :^(")
        return(NULL)
    }
    
}

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
print(corenum)
options(future.globals.maxSize= 48.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr)

future_lapply(
    target_hucs,
    process_huc,
    future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
    future.globals = TRUE,
    future.seed = TRUE  
)

# ### Non-parallel testing
# lapply(target_hucs[[1]], process_huc)




