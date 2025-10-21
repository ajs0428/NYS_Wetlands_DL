#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
    11,
    "Data/CHMs/AWS"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
     "- Path to a file unprocessed NAIP files:", args[1], "\n",
     "- Path to processed NAIP files:", args[2], "\n"
))

###############################################################################################
library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)
###############################################################################################

cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)
###############################################################################################

# This is all the CHM file names 
# chms_file_list <- list.files("Data/CHMs/AWS/", 
#                              pattern = ".tif", 
#                              full.names = TRUE, 
#                              recursive = TRUE, 
#                              include.dirs = FALSE)
# chms_file_list_limit <- chms_file_list[sapply(chms_file_list, file.size) > 500E3]
# chms_file_list_limit_base <- sub(".*AWS//", "", chms_file_list_limit)
# 
# saveRDS(chms_file_list, "Data/CHMs/chms_file_list.rds")
# saveRDS(chms_file_list_limit, "Data/CHMs/chms_file_list_limit.rds")
# saveRDS(chms_file_list_limit_base, "Data/CHMs/chms_file_list_limit_base.rds")

chms_file_list <- readRDS("Data/CHMs/chms_file_list.rds")
chms_file_list_limit <- readRDS("Data/CHMs/chms_file_list_limit.rds")
chms_file_list_limit_base <- readRDS("Data/CHMs/chms_file_list_limit_base.rds")

print(paste0("this is the total list of chm indexes: ", length(chms_file_list_limit)[[1]]))


chms_gpkg_list <- list.files(args[3],
                             pattern = ".gpkg$",
                             full.names = TRUE,
                             recursive = FALSE)
print(chms_gpkg_list)
###############################################################################################
all_crossing_features <- list()

for (i in seq_along(chms_gpkg_list)) {
    cat("Processing file", i, "of", length(chms_gpkg_list), "\n")
    
    # Read CHM locations
    features <- st_read(chms_gpkg_list[i], quiet = TRUE)
    features_locs_base <- sub(".*AWS//", "", features$location)
    features <- features[(features_locs_base %in% chms_file_list_limit_base),]
    
    # Transform to common CRS
    if (!st_crs(features) == st_crs(cluster_target)) {
        cat("  Transforming features to match polygon CRS...\n")
        features <- st_transform(features, st_crs(cluster_target))
    }
    
    features_in_cluster <- st_filter(features, cluster_target, .predicate = st_intersects) 
    all_crossing_features[[i]] <- features_in_cluster
    rm(features)
    rm(features_in_cluster)
}

final_crossing_features <- dplyr::bind_rows(all_crossing_features)

# final_crossing_features_rasts <- paste0(args[3], "/", final_crossing_features$location)
# final_crossing_features_vrt <- vrt(final_crossing_features_rasts) |> 
#                                 terra::project("EPSG:6347")
# ###############################################################################################
# 
# cluster_chm_extract <- terra::crop(final_crossing_features_vrt, cluster_target |> vect(),
#                                    mask = TRUE)
###############################################################################################


for(i in seq_along(cluster_target$huc12)){
    cluster_huc_name <- cluster_target$huc12[[i]]
    print(cluster_huc_name)
    
    huc_chms <- st_filter(final_crossing_features, cluster_target[i,], .predicate = st_intersects) 
    huc_rasts <- paste0(args[3], "/",  huc_chms$location)
    huc_chm_vrt <- terra::vrt(huc_rasts) |> 
        terra::project("EPSG:6347", res = 1) |> 
        terra::crop(y = cluster_target[i,], mask = TRUE, 
                    filename = paste0("Data/CHMs/HUC_CHMs", "/cluster_", args[2], "_huc_", cluster_huc_name, "_CHM.tif"))
}
