#!/usr/bin/env Rscript

args = c(
         "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         208,
         "Data/TerrainProcessed/HUC_TerrainMetrics/",
         "Data/TerrainProcessed/HUC_Hydro/",
         "Data/NAIP/NAIP_HUC_Merged/",
         "no",
         "Data/Training_Data/Cluster_Extract_Training_Pts/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area:", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path Terrain Metrics", args[3], "\n",
    "- Path to Hydrology Metrics:", args[4], "\n",
    "- Path to NAIP Imagery:", args[5], "\n",
    "- Need a new predictor stack? (yes/no):", args[6], "\n",
    "- Path to Export:", args[7], "\n"
)

###############################################################################################

library(terra)
library(sf)
library(here)
library(foreach)
library(doParallel)
library(collapse)
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(tidyverse))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 

###############################################################################################
ny_pts <- list.files("Data/Training_Data", 
                     pattern = paste0("cluster_", args[2],"_training_pts.gpkg"), 
                     full.names = TRUE) |> 
    lapply(sf::st_read, quiet = TRUE) |> 
    lapply(sf::st_transform, st_crs("EPSG:6347")) |> 
    dplyr::bind_rows()

# ny_pts <- vect("Data/Training_Data/Northern_Allegheny_Plateau_training_pts.gpkg") |> 
#     terra::project("EPSG:6347")

pts_list <- list()
for(i in 1:nrow(cluster_target["huc12"])){
    huc_name <- cluster_target[i,]["huc12"][[1]]
    huc <- cluster_target |> dplyr::filter(huc12 == huc_name)
    pts_list[[i]] <- sf::st_filter(ny_pts, huc)
}
names(pts_list) <- as.vector(cluster_target["huc12"][[1]])
###############################################################################################
#pts_list[1:2] 
dem_list <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".*\\d+.tif", full.names = TRUE)
terr_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], ".*\\m.tif"), full.names = TRUE)
hydro_list <- list.files(path = args[4], pattern = paste0("cluster_", args[2], ".*\\.tif"), full.names = TRUE)
naip_huc_list <- list.files(path = args[5], pattern = paste0(".*\\cluster_", args[2], ".*\\.tif"), full.names = TRUE)

raster_stack_extract <- function(terr_list, hydro_list, naip_list, pts_list){
    # cl <- makeCluster(8)
    # registerDoParallel(cl)
    # 
    # pts_extract_list <- foreach(i = seq_along(pts_list), 
    #                             .packages = c("terra", "tidyterra", "stringr"),
    #                             .export = c("args", "terr_list", "hydro_list", "pts_list")) %dopar% {
    pts_extract_list <- list()
    for(i in seq_along(pts_list)){
        huc_name <- names(pts_list[i])
        print(huc_name)
        
        #print(ext(cr))
        if(stringr::str_detect(tolower(args[6]), "yes")){
            print("rewriting/creating raster stacks")
            dr <- dem_list[str_detect(dem_list, huc_name)] |> rast()
            tr <- terr_list[str_detect(terr_list, huc_name)] |> rast()
            hr <- hydro_list[str_detect(hydro_list, huc_name)] |> rast()
            nr <- naip_huc_list[str_detect(naip_huc_list, huc_name)] |> rast() |> 
                terra::resample(tr, method = "bilinear", threads = TRUE)
            # print(ext(tr))
            # print(ext(hr))
            # print(ext(nr))
            cr <- c(tr, hr, nr)
            writeRaster(cr,
                        filename = paste0("Data/Predictor_Stacks/cluster_",
                                          args[2], "_huc_", huc_name, "_raster_predictors.tif"),
                        overwrite = TRUE)
        } else {
            print("predictor stacks exist")
            cr <- rast(paste0("Data/Predictor_Stacks/cluster_",
                              args[2], "_huc_", huc_name, "_raster_predictors.tif"))
        }
        

        pts_extract_list[[i]] <- terra::extract(cr, terra::vect(pts_list[[i]]), method = "bilinear", bind = TRUE)

    }
    # print(pts_extract_list)
    pts_extract <- vect(pts_extract_list)
    # print(pts_extract)
    writeVector(pts_extract, filename = paste0(args[7], "cluster_", args[2], "_extracted_training_pts.gpkg"),
                overwrite = TRUE)

    #stopCluster(cl)
}


raster_stack_extract(terr_list = terr_list, 
                     hydro_list = hydro_list, 
                     naip_list = naip_huc_list,
                     pts_list = pts_list)


# tr <- terr_list[str_detect(terr_list, "041402011002")] |> rast()
# hr <- hydro_list[str_detect(hydro_list, "041402011002")] |> rast()
# nr <- naip_huc_list[str_detect(naip_huc_list, "041402011002")] |> rast()
# # nrs <- terra::resample(nr, tr)
# 
# plot(nr)
