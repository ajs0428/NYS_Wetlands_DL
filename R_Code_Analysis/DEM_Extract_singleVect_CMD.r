#!/usr/bin/env Rscript

###################
# This script creates a DTM mosaic for each HUC in a cluster 
    # The cluster is pre-defined as a group of HUCs
###################

args = c("Data/NYS_DEM_Indexes/",
         "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         50,
         "Data/DEMs/",
         "Data/TerrainProcessed/HUC_DEMs"
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to the DEM indexes folder", args[1], "\n",
    "- Path to a file vector study area", args[2], "\n",
    "- Cluster number (integer 1-75):", args[3], "\n",
    "- Path to DEM folder:", args[4], "\n", 
    "- Path to Save folder:", args[5], "\n"
    )
###############################################################################################

library(terra)
library(sf)
library(MultiscaleDTM)
library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(stringr)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,
             memmax = 8,
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")

###############################################################################################

# A shapefile list of all the DEM indexes (vector tiles of the actual DEM locations)
dem_ind_list <- (lapply(list.files(args[1], pattern = ".shp$",full.names = TRUE), sf::st_read))

transform_sf <- function(stl){
    new_stl <- list()
    for(i in seq_along(stl)){
        if(crs(stl[[i]]) != st_crs("EPSG:6347")){
            new_stl[[i]] <- st_transform(stl[[i]], st_crs("EPSG:6347"))
        } else {
            new_stl[[i]] <- stl[[i]]
        }
    }
    return(new_stl)
}

dem_ind_trs <- transform_sf(dem_ind_list)

# This should just output a summary for the collections
for(i in dem_ind_trs) {print(st_crs(i)$input)}

###############################################################################################
# This is all the DEM file names 
dems_file_list <- list.files(args[4], 
                             pattern = ".img$|.tif$", 
                             full.names = TRUE, 
                             recursive = TRUE, 
                             include.dirs = TRUE)
print(paste0("this is the total list of DEM indexes: ", length(dems_file_list)[[1]]))
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
    # of interest.
    # cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[2]) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[3])

cluster_extract <- function(cluster, dem_indexes){
    files_to_extract <- list()
    for(i in seq_along(dem_indexes)){
        dems_in <- dem_indexes[[i]] |> st_filter(y = cluster, .predicate = st_intersects)
        if(nrow(dems_in) > 1){
            Fnames <- dems_in["FILENAME"][[1]]
            files_to_extract <- append(files_to_extract, Fnames)
        } else {
            next
        }
    }
    return(files_to_extract)
}

cluster_dem_indices <- cluster_extract(cluster_target, dem_ind_trs)

#Take the list of cluster_dem_indices and go by each HUC to merge into a full raster    
    # should return a list of rasters that can be merged again

f_list <- list() # list of lists of filenames 
v_list <- list() # list for HUC vectors
for(i in seq_along(cluster_target$huc12)){
    dem_to_extr <- cluster_extract(cluster = cluster_target[i, ], dem_indexes = dem_ind_trs)
    f_list[[i]] <- dems_file_list[basename(dems_file_list) %in% dem_to_extr]
    v_list[[i]] <- terra::vect(cluster_target[i,]) |>
        terra::project(crs("EPSG:6347")) |> terra::wrap()
}

huc_extract <- function(list_of_files, list_of_vectors){
    print(length(list_of_files))
    huc_name <- (terra::unwrap(list_of_vectors)[1,"huc12", drop = T][[1]])
    print(paste0(args[5], "/cluster_", args[3], "_huc_", huc_name,".tif"))
    
    subfolders_names <- basename(dirname(list_of_files))
    file_lists <- split(list_of_files, subfolders_names)
    lvrt <- lapply(file_lists, function(lists){
        terra::vrt(lists) |> 
            terra::project("EPSG:6347", res = 1)
    })
    
    tryCatch(
        {terra::merge(sprc(lvrt)) |>
        terra::mask(mask = terra::unwrap(list_of_vectors), updatevalue = NA, touches = TRUE,
                    filename = paste0(args[5], "/cluster_", args[3], "_huc_", huc_name,".tif"),
                    overwrite = TRUE)
            }, 
        error = function(msg){
            message(paste0("Error at: ", huc_name))
                        return(NA)
            }
        )

}

mapply(huc_extract, f_list, v_list)

# huc_extract(cluster = cluster_target)

# corenum <-  4
# options(future.globals.maxSize= 8.0 * 1e9)
# plan(multisession, workers = corenum) 
# 
# print(corenum)
# print(options()$future.globals.maxSize)
# 
# future_mapply(huc_extract, f_list[1:2], v_list[1:2], future.seed = TRUE)

##########################################

