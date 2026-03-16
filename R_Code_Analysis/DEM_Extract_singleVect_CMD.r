#!/usr/bin/env Rscript

###################
# This script creates a DTM mosaic for each HUC in a cluster 
# The cluster is pre-defined as a group of HUCs
###################

args = c("Data/NYS_DEM_Indexes",
         "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
         11,
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
library(dplyr)
library(tidyr)
library(future)
library(future.apply)
library(stringr)
suppressPackageStartupMessages(library(tidyterra))

# Configure terra for efficiency
terraOptions(
    tempdir = "/ibstorage/anthony/NYS_Wetlands_DL/Data/tmp",
    memfrac = 0.6,      # Use up to 60% of RAM before writing to disk
    threads = 2         # Internal threading for terra operations (per worker)
)
###############################################################################################

# A shapefile list of all the DEM indexes (vector tiles of the actual DEM locations)
dem_ind_list <- list.files(args[1], pattern = "^dem_1_meter.*\\.shp$|USGS_LakeOntarioHudsonRiverRegion2022|FEMA_Bare_Earth_DEM_1m.shp",full.names = TRUE)

dem_ind_full <- lapply(dem_ind_list, st_read, quiet = TRUE) |> 
    lapply(\(x) st_transform(x, "EPSG:6347")) |> 
    lapply(\(x) st_make_valid(x)) |> 
    lapply(\(x) dplyr::select(x, any_of(c("FILENAME", "location")))) |> 
    bind_rows() |> 
    dplyr::mutate(FilenameCmb = case_when(is.na(location) ~ FILENAME,
                                          .default = location),
                  geom = case_when(st_is_empty(geom) ~ geometry,
                                   .default = geom)) |> 
    dplyr::select(FilenameCmb, geom) |> 
    st_set_geometry("geom") |> 
    dplyr::select(-geometry)

# dem_ind_full_int <- st_overlaps(dem_ind_full)
# first_occurrence <- sapply(seq_along(dem_ind_full_int), \(x) min(dem_ind_full_int[[x]]) != x)
# dem_ind_full_fix <- dem_ind_full[first_occurrence, ]


print(paste0("The number of DEM indices: ", nrow(dem_ind_full)))


###############################################################################################
# This is all the DEM file names 
dems_file_list <- list.files(args[4], 
                             pattern = ".img$|.tif$", 
                             full.names = TRUE, 
                             recursive = TRUE, 
                             include.dirs = TRUE)
print(paste0("this is the total list of DEM raster files: ", length(dems_file_list)[[1]]))


ind_names <- tools::file_path_sans_ext(basename(dem_ind_full$FilenameCmb))
dem_names <- tools::file_path_sans_ext(basename(dems_file_list))

# (dem_ind_full$FilenameCmb[!ind_names %in% dem_names])
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[2], quiet = TRUE) |> 
    dplyr::filter(cluster == args[3])
cluster_hucs <- cluster_target$huc12

cluster_extract <- function(huc){
    huc_sf <- cluster_target[cluster_target$huc12 == huc, ]
    
    dem_ind_huc <- dem_ind_full[rowSums(st_intersects(dem_ind_full, huc_sf, sparse = FALSE)) != 0,] # |> 
        # filter(as.numeric(st_area(geom)) > 2000000)
    Fnames <- tools::file_path_sans_ext(basename(dem_ind_huc$FilenameCmb))
    
    dems_fn_huc <- dems_file_list[tools::file_path_sans_ext(basename(dems_file_list)) %in% Fnames]
    
    huc_dem_fn <- (paste0(args[5], "/cluster_", args[3], "_huc_", huc,".tif"))
    if(file.exists(huc_dem_fn)){
        message("Create raster for ", huc_dem_fn)
        huc_vect <- vect(huc_sf)
        lvrt <-  lapply(dems_fn_huc, terra::rast) |> 
            lapply(terra::project, y = "EPSG:6347", res = 1) |>
            terra::sprc() |>
            terra::mosaic(fun = "first") |>
            terra::focal(na.policy = "only", fun = "mean", w = 3)
        set.names(lvrt, "DEM")
        dem <- terra::crop(lvrt, huc_vect, mask = TRUE)
        # dem[is.infinite(dem)] <- NA
        writeRaster(dem,
                    filename = huc_dem_fn,
                    overwrite = TRUE)
    } else {
        message("DEM already exists at ", huc_dem_fn)
    }
    gc()
}

# lapply(cluster_hucs[[7]], cluster_extract)

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)
# 
future_lapply(cluster_hucs,
              cluster_extract,
              future.seed = TRUE,
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
              future.globals = TRUE)

##########################################

