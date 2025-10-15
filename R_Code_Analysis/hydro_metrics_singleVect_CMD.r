#!/usr/bin/env Rscript

###################
# This script creates a "hydro-conditioned" DTM for hydrologic modeling
    # The new DTMs are named with 'wbt' for Whitebox Tools
# It also creates the hydrologic metrics for Topographic Wetness Index
###################

args = c("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
         11,
         "Data/TerrainProcessed/HUC_DEMs/",
         "Data/TerrainProcessed/HUC_Hydro/"
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

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
library(future)
library(future.apply)
library(stringr)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.25,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() 

###############################################################################################
# All the DEMs in a cluster
list_of_huc_dems <- list.files(args[3], 
                               full.names = TRUE, 
                               glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*.tif$"))
                               )
# All the hydro-conditioned DEMs
wbt_list <- list_of_huc_dems[stringr::str_detect(list_of_huc_dems, pattern = "wbt")]
wbt_hucs <- str_extract(wbt_list, "(?<=huc_)\\d+")
# All the non-hydro-conditioned DEMs
non_wbt_list <- list_of_huc_dems[stringr::str_detect(list_of_huc_dems, pattern = "wbt", negate = T)]
non_wbt_hucs <- str_extract(non_wbt_list, "(?<=huc_)\\d+")

#HUCs that haven't been hydroconditioned
missing_wbt_hucs <- setdiff(non_wbt_hucs, wbt_hucs)

hydro_condition_func <- function(dem){
    print("Hydro-Conditioning")
    wbt_breach_depressions_least_cost(
        dem = dem,
        output = paste0(stringr::str_remove(dem, ".tif"), "_wbt.tif"),
        dist = 100,
        fill = TRUE
    )
}

if(length(wbt_list) < length(non_wbt_list)){
    print(paste0("Requires Hydro-Conditioning: ", length(non_wbt_list)))
    cond_list <- non_wbt_list[!(non_wbt_hucs %in% wbt_hucs)]
    future_lapply(cond_list, hydro_condition_func, future.seed = TRUE)
} else {
    print(paste0("Already Hydro-Conditioned Number of Hydro-DEMs: ", length(wbt_list)))
}



###############################################################################################

list_of_huc_hydro_dems <- list.files(args[3], 
                                     full.names = TRUE, 
                                     glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*wbt.tif$"))
                                     )

hydro_func <- function(dem){
                
    cluster_huc_name <- stringr::str_remove(basename(dem), ".tif")
    #dems_target <- dems_list[[i]]
    
    fs <- dem |>
        terra::rast() |>
        terra::terrain(v = c("flowdir", "slope"), unit = "radians")
    fa <- terra::flowAccumulation(fs["flowdir"])

    twi <- log(fa/tan(fs["slope"]))

    writeRaster(c(fa, twi), paste0(args[4], cluster_huc_name, "_TWI_Facc.tif"),
                overwrite = TRUE, names = c("flowacc", "twi"))

}
# 
# corenum <-  4
# options(future.globals.maxSize= 16 * 1e9)
# plan(multisession, workers = corenum) 
# 
# print(corenum)
# print(options()$future.globals.maxSize)
# 
# future_lapply(list_of_huc_hydro_dems, hydro_func, future.seed = TRUE)

lapply(list_of_huc_hydro_dems, hydro_func)

# r <- rast(list_of_huc_hydro_dems[[1]])
# s <- terra::terrain(r, v = "slope")
# w <- terra::unwrap(cluster_target) |> 
#     tidyterra::filter(huc12 == str_extract(list_of_huc_hydro_dems[[1]], "(?<=huc_)\\d+")) |> 
#     terra::crop(x = vect("Data/Hydrography/NHD_NYS_wb_area.gpkg")) |> 
#     terra::mask(x = s, inverse = TRUE, updatevalue = -999)
# 
# cd <- costDist(w, target = -999, maxiter = 1)
