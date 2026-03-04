#!/usr/bin/env Rscript

args = c(
         "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg", #1
         11, #2
         "Data/TerrainProcessed/HUC_TerrainMetrics/", #3
         "Data/TerrainProcessed/HUC_Hydro/", #4
         "Data/NAIP/HUC_NAIP_Processed/", #5
         "Data/CHMs/HUC_CHMs/", #6
         "Data/Satellite/HUC_Processed_NY_Sentinel_Indices/", #7
         "Data/Training_Data/HUC_Extracted_Training_Data/" #8
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here
 
cat("these are the arguments: \n", 
    "- Path to a file vector study area:", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path Terrain Metrics", args[3], "\n",
    "- Path to Hydrology Metrics:", args[4], "\n",
    "- Path to NAIP Imagery:", args[5], "\n",
    "- Path to CHMs:", args[6], "\n",
    "- Path to Export:", args[7], "\n"
)

###############################################################################################

library(terra)
library(sf)
library(here)
library(collapse)
library(future)
library(future.apply)
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(tidyverse))

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 

###############################################################################################
ny_pts <- list.files("Data/Training_Data", 
                     pattern = paste0("cluster_", args[2], "(_.*)?_training_pts.gpkg$"), 
                     full.names = TRUE) |> 
    lapply(sf::st_read, quiet = TRUE) |> 
    dplyr::bind_rows()

pts_list <- list()
for(i in 1:nrow(cluster_target["huc12"])){
    huc_name <- cluster_target[i,]["huc12"][[1]]
    huc <- cluster_target |> dplyr::filter(huc12 == huc_name)
    pts_list[[i]] <- sf::st_filter(ny_pts, huc)
}
names(pts_list) <- as.vector(cluster_target["huc12"][[1]])
print(names(pts_list))
###############################################################################################

dem_list <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = paste0("cluster_", args[2], "_", ".*\\d+.tif"), full.names = TRUE)
terr_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], "_", ".*\\m.tif"), full.names = TRUE) %>% 
    .[!str_detect(., "1000m|10m")]
hydro_list <- list.files(path = args[4], pattern = paste0("cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
naip_list <- list.files(path = args[5], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
chm_list <- list.files(path = args[6], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
sat_list <- list.files(path = args[7], pattern = paste0(".*\\cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)


###############################################################################################
raster_stack_extract <- function(dem){
    
    huc_name <- str_extract(dem, "(?<=huc_)\\d+")
    dem_rast <- rast(dem)
    set.names(dem_rast, "DEM")
    terr_rast <- terr_list[str_detect(terr_list, huc_name)] |> rast()
    hydro_rast <- hydro_list[str_detect(hydro_list, huc_name)] |> rast()
    naip_rast <- naip_list[str_detect(naip_list, huc_name)] |> rast()
    names(naip_rast) <- paste0("naip_", names(naip_rast))
    chm_rast <- chm_list[str_detect(chm_list, huc_name)] |> rast()
    sat_rast <- sat_list[str_detect(sat_list, huc_name)] |> rast()
    names(sat_rast) <- paste0("sat_", names(sat_rast))
    pts <- pts_list[huc_name][[1]]

    pts_extracted <- terra::extract(y = pts,
                                    x = dem_rast,
                                    bind = TRUE) |>
        terra::extract(x = terr_rast,
                       bind = TRUE) |>
        terra::extract(x = hydro_rast,
                       bind = TRUE) |>
        terra::extract(x = naip_rast,
                       bind = TRUE ) |>
        terra::extract(x = chm_rast,
                       bind = TRUE) |>
        terra::extract(x = sat_rast,
                       bind = TRUE) |>
        tidyterra::mutate(huc = huc_name,
                          cluster = args[2])

    writeVector(pts_extracted, filename = paste0(args[8],
                                                 "cluster_",
                                                 args[2],
                                                 "_huc_",
                                                 huc_name,
                                                 "ext_train_pts.gpkg"),
                overwrite =TRUE)

    rm(dem_rast)
    rm(terr_rast)
    rm(chm_rast)
    rm(hydro_rast)
    rm(naip_rast)
    rm(sat_rast)
    rm(pts_extracted)
    gc(verbose = FALSE)

}

###############################################################################################

# lapply(dem_list, raster_stack_extract)

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

Rprof("Shell_Scripts/logs/profile.out", memory.profiling = TRUE)
(future_lapply(dem_list, raster_stack_extract))
Rprof(NULL)
summaryRprof("Shell_Scripts/logs/profile.out", memory = "both")
