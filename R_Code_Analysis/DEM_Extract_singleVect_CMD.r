#!/usr/bin/env Rscript

###################
# This script creates a DTM mosaic for each HUC in a cluster 
    # The cluster is pre-defined as a group of HUCs
###################

args = c("Data/NYS_DEM_Indexes/",
         "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
         120,
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


###############################################################################################

# A shapefile list of all the DEM indexes (vector tiles of the actual DEM locations)
dem_ind_list <- (lapply(list.files(args[1], pattern = "^dem_1_meter.*\\.shp$|USGS_LakeOntarioHudsonRiverRegion2022",full.names = TRUE), sf::st_read, quiet = TRUE))


transform_sf <- function(stl){
    new_stl <- list()
    for(i in seq_along(stl)){
        stl_fix <- stl[[i]][st_is_valid(dem_ind_list[[1]]), ]
        if(crs(stl[[i]]) != st_crs("EPSG:6347")){
            #stl_fix <- st_make_valid(stl[[i]])
            new_stl[[i]] <- st_transform(stl_fix, st_crs("EPSG:6347")) 
        } else {
            #stl_fix <- st_make_valid(stl[[i]])
            new_stl[[i]] <- stl_fix
        }
    }
    return(new_stl)
}

dem_ind_trs <- transform_sf(dem_ind_list)

# dem_ind_trs <- st_transform(dem_ind_list, st_crs("EPSG:6347"))
# print(paste0("The number of DEM indices: ", nrow(dem_ind_trs)))


# This should just output a summary for the collections
for(i in dem_ind_trs) {print(st_crs(i)$input)}

###############################################################################################
# This is all the DEM file names 
dems_file_list <- list.files(args[4], 
                             pattern = ".img$|.tif$", 
                             full.names = TRUE, 
                             recursive = TRUE, 
                             include.dirs = TRUE)
print(paste0("this is the total list of DEM raster files: ", length(dems_file_list)[[1]]))
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
    # of interest.
    # cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[2], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[3])

cluster_extract <- function(cluster, dem_indexes){
    files_to_extract <- list()
    for(i in seq_along(dem_indexes)){
        # dems_in <- dem_indexes[[i]] |> st_filter(y = cluster, .predicate = st_intersects)
        dems_in <- tryCatch(
            dem_indexes[[i]] |> st_filter(y = cluster, .predicate = st_intersects),
            error = function(e) {
                # Return an empty sf object with 0 rows
                st_sf(geometry = st_sfc(crs = st_crs(dem_indexes[[i]])))
            }
        )
        if(nrow(dems_in) > 1){
            Fnames <- tryCatch(
                dems_in["FILENAME"][[1]],
                error = function(e){
                    basename(dems_in["location"][[1]])
                }
            )
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

files <- Filter(Negate(is.null), f_list)
vectors <- Filter(Negate(is.null), v_list)
print(length(files))
print(length(vectors))

huc_extract <- function(dem_files, vect_files){

    huc_name <- (terra::unwrap(vect_files)[1,"huc12", drop = T][[1]])

    fn <- (paste0(args[5], "/cluster_", args[3], "_huc_", huc_name,".tif"))
    print(fn)

    if(!file.exists(fn)){
    print(paste0("Processing new file: ", fn))
    # subfolders_names <- basename(dirname(dem_files)) #the DEMs are different collection folders
    # file_lists <- unlist(dem_files) # split(dem_files, subfolders_names)
    
    tryCatch(
        {
    lvrt <-  terra::mosaic(terra::sprc(dem_files), fun = "mean") |>
        terra::project("EPSG:6347", res = 1)
    set.names(lvrt, "DEM")
    terra::mask(lvrt, mask = terra::unwrap(vect_files), updatevalue = NA, touches = TRUE,
            filename = fn,
            overwrite = TRUE)
            },
        error = function(msg){
            message(msg$message)
            message(paste0("Error at: ", huc_name))
                        return(NA)
            }
        )
    } else {
        print(paste0("File already exists: ", fn))
    }
}

mapply(huc_extract, files, vectors)

# huc_extract(cluster = cluster_target)

# if(future::availableCores() > 16){
#     corenum <-  4
# } else {
#     corenum <-  (future::availableCores())
# }
# options(future.globals.maxSize= 64 * 1e9)
# # plan(multisession, workers = corenum)
# plan(future.callr::callr, workers = corenum)
# 
# future_mapply(huc_extract, f_list[1:2], v_list[1:2], future.seed = TRUE)

##########################################

