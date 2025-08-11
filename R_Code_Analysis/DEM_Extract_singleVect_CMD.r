#!/usr/bin/env Rscript

args = c("Data/NYS_DEM_Indexes/",
         "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         208,
         "Data/DEMs/",
         "Data/TerrainProcessed/"
         )
# args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

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
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,
             memmax = 8,
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
cl <- makeCluster(4)
registerDoParallel(cl)
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

#Take the list of dems_to_extract and go by each HUC to merge into a full raster    
    # should return a list of rasters that can be merged again
huc_extract <- function(cluster){
    f_list <- list() # list of lists of filenames 
    v_list <- list() # list for HUC vectors
    for(i in seq_along(cluster$huc12)){
        dem_to_extr <- cluster_extract(cluster = cluster[i, ], dem_indexes = dem_ind_trs)
        f_list[[i]] <- dems_file_list[basename(dems_file_list) %in% dem_to_extr]
        v_list[[i]] <- terra::vect(cluster[i,]) |>
            terra::project(crs("EPSG:6347")) |> terra::wrap()
    }
    
    # list of lists of spatrasterCollections
    r_list <- foreach(i = seq_along(cluster$huc12),
                      .packages = c("terra", "tidyterra", "sf"),
                      .export = "args") %dopar% {
        f_list[[i]] |> 
            as.list() |> 
            lapply(terra::rast) |> 
            terra::sprc() |> 
            terra::merge() |>
            terra::mask(mask = terra::unwrap(v_list[[i]]), updatevalue = NA, touches = TRUE,
                        filename = paste0(args[5], "cluster_", args[3], "_huc_", cluster[["huc12"]][[i]],".tif"),
                        overwrite = TRUE) #|>
            #terra::wrap()
        #return(rs)
    }
    #return(list(f_list,lapply(v_list, terra::unwrap), lapply(r_list, terra::unwrap)))
}

#cluster_target3 <- cluster_target[1:3]

huc_extract(cluster = cluster_target)


stopCluster(cl)
