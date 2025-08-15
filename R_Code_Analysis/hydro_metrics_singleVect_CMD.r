#!/usr/bin/env Rscript

args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         208,
         "Data/TerrainProcessed/HUC_DEMs/",
         "Data/TerrainProcessed/HUC_Hydro/"
         )
# args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Path to save folder:", args[4], "\n"
)

###############################################################################################

library(terra)
library(sf)
library(whitebox)
library(MultiscaleDTM)
library(foreach)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 8, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1]) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() |> 
    terra::wrap()

###############################################################################################
# list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "*\\*.tif$")))
# 
# 
# hydro_condition_func <- function(dems_list){
#     cl <- makeCluster(4)
#     registerDoParallel(cl)
#     
#     foreach(i = seq_along(dems_list), 
#             .packages = c("terra", "whitebox"),
#             .export = c("args")) %dopar% {
#                 
#         if(file.exists(paste0(stringr::str_remove(dems_list[[i]], "_wbt.tif"), "_wbt.tif"))){
#             
#             print("matched file")
#             
#         } else {
#             print("I'm doing it")
#             # wbt_breach_depressions_least_cost(
#             #     dem = dems_list[[i]],
#             #     output = paste0(stringr::str_remove(dems_list[[i]], ".tif"), "_wbt.tif"),
#             #     dist = 100,
#             #     fill = TRUE,
#             # )
#         }
#     }
#     stopCluster(cl)
# }
# 
# t <- hydro_condition_func(list_of_huc_dems)

###############################################################################################

list_of_huc_hydro_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "*\\*wbt.tif$")))

hydro_func <- function(dems_list){
    cl <- makeCluster(8)
    registerDoParallel(cl)
    
    foreach(i = seq_along(dems_list), 
            .packages = c("terra", "tidyterra", "whitebox"),
            .export = c("args")) %dopar% {
                
                cluster_huc_name <- stringr::str_remove(basename(dems_list[[i]]), ".tif")
                #dems_target <- dems_list[[i]]
                
                fs <- dems_target |>
                    terra::rast() |>
                    terra::terrain(v = c("flowdir", "slope"), unit = "radians")
                fa <- terra::flowAccumulation(fs["flowdir"])

                twi <- log(fa/tan(fs["slope"]))

                writeRaster(c(fa, twi), paste0(args[4], cluster_huc_name, "_TWI_Facc.tif"),
                            overwrite = TRUE, names = c("flowacc", "twi"))

                rm(cluster_huc_name, dems_target, fs)
            }
    stopCluster(cl)
}

hydro_func(list_of_huc_dems)

r <- rast(list_of_huc_hydro_dems[[1]])
s <- terra::terrain(r, v = "slope")
w <- terra::unwrap(cluster_target) |> 
    tidyterra::filter(huc12 == str_extract(list_of_huc_hydro_dems[[1]], "(?<=huc_)\\d+")) |> 
    terra::crop(x = vect("Data/Hydrography/NHD_NYS_wb_area.gpkg")) |> 
    terra::mask(x = s, inverse = TRUE, updatevalue = -999)

cd <- costDist(w, target = -999, maxiter = 1)
