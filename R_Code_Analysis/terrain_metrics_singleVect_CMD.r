#!/usr/bin/env Rscript

args = c("Data/NYS_DEM_Indexes/",
         "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         190,
         "Data/DEMs/",
         "Data/TerrainProcessed/",
         11
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to the DEM indexes folder", args[1], "\n",
    "- Path to a file vector study area", args[2], "\n",
    "- Cluster number (integer 1-75):", args[3], "\n",
    "- Path to DEM folder:", args[4], "\n", 
    "- Path to Save folder:", args[5], "\n", 
    "- Odd Integer:", args[6], "\n"
    )
###############################################################################################
library(terra)
library(sf)
library(MultiscaleDTM)
#library(future)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.25,
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")

###############################################################################################

# A shapefile list of all the DEM indexes (vector tiles of the actual DEM locations)
dem_ind_list <- (lapply(list.files(args[1], pattern = ".shp$",full.names = TRUE), sf::st_read))

transform_sf <- function(stl){
    new_stl <- list()
    for(i in seq_along(stl)){
        if(crs(stl[[i]]) != st_crs("EPSG:26918")){
            new_stl[[i]] <- st_transform(stl[[i]], st_crs("EPSG:26918"))
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
# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
    # of interest
cluster_target <- sf::st_read(args[2]) |> 
    sf::st_transform(st_crs("EPSG:26918")) |>
    dplyr::filter(cluster == args[3])

cluster_extract <- function(cluster, dem_list){
    files_to_extract <- list()
    for(i in seq_along(dem_list)){
        dems_in <- dem_list[[i]] |> st_filter(y = cluster, .predicate = st_intersects)
        if(nrow(dems_in) > 1){
            Fnames <- dems_in["FILENAME"][[1]]
            files_to_extract <- append(files_to_extract, Fnames)
        } else {
            next
        }
    }
    return(files_to_extract)
}

dem_ind_extr <- cluster_extract(cluster = cluster_target, dem_list = dem_ind_trs)
###############################################################################################

dems_file_list <- list.files(args[4], 
                            pattern = ".img$|.tif$", 
                            full.names = TRUE, 
                            recursive = TRUE, 
                            include.dirs = TRUE)

dems_to_extract <- dems_file_list[basename(dems_file_list) %in% dem_ind_extr]

#Take the list of dems_to_extract and go by each HUC to merge into a full raster    
    # should return a list of rasters that can be merged again
huc_extract_merge <- function(cluster){
    f_list <- list()
    for(i in seq_along(cluster$huc12)){
        dem_to_extr <- cluster_extract(cluster = cluster[i, ], dem_list = st_list_trs)
        f_list[[i]] <- dem_file_list[basename(dem_file_list) %in% dem_to_extr]
    }
    
    r_list <- list()
    for(i in seq_along(cluster$huc12)){
        r_col <- terra::sprc(f_list[[i]])
        r_list[[i]] <- terra::merge(r_col)
    }
    return(r_list)
}


writeRaster(dems_target, paste0(args[5],"cluster_",args[3], "_dem.tif"),
            overwrite = TRUE)#This should write a raster as a DEM mosaic 

###############################################################################################

win <- c(as.numeric(args[6]), as.numeric(args[6]))

slp <- MultiscaleDTM::SlpAsp(dems_target, w = win, unit = "degrees", 
                                         include_scale = TRUE, metrics = "slope")
curv <- MultiscaleDTM::Qfit(dems_target, w = win,
                             include_scale = TRUE, metrics = c("meanc", "planc", "profc"))
dmv <- MultiscaleDTM::DMV(dems_target, w = win, stand = "none", # I think "none" so that NA won't be produced
                   include_scale = TRUE)

writeRaster(c(slp, curv, dmv), filename = paste0(args[5], "_slp_curv_dmv_", args[6],".tif"),
            overwrite = TRUE)