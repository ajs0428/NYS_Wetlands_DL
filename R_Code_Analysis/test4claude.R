#!/usr/bin/env Rscript

###################
# This script creates a "hydro-conditioned" DTM for hydrologic modeling
# The new DTMs are named with 'wbt' for Whitebox Tools
# It also creates the hydrologic metrics for Topographic Wetness Index
###################

args = c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
         120,
         "Data/TerrainProcessed/HUC_DEMs/",
         "Data/TerrainProcessed/HUC_Hydro/"
)

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

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())

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
print(missing_wbt_hucs)

hydro_condition_func <- function(dem){
    print("Hydro-Conditioning")
    wbt_breach_depressions_least_cost(
        dem = dem,
        output = paste0(stringr::str_remove(dem, ".tif"), "_wbt.tif"),
        dist = 100,
        fill = TRUE
    )
}

if(length(missing_wbt_hucs) != 0){
    print(paste0("Requires Hydro-Conditioning: ", length(missing_wbt_hucs)))
    cond_list <- non_wbt_list[!(non_wbt_hucs %in% wbt_hucs)] # the DEMs that need hydrocondition not in the wbt list
    print(cond_list)
    lapply(cond_list, hydro_condition_func)
} else {
    print(paste0("Already Hydro-Conditioned Number of Hydro-DEMs: ", length(wbt_list)))
}



###############################################################################################

list_of_huc_hydro_dems <- list.files(args[3], 
                                     full.names = TRUE, 
                                     glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*wbt.tif$"))
)
list_of_huc_hydro_dems
hydro_func <- function(dem){
    
    cluster_huc_name <- stringr::str_remove(basename(dem), ".tif")
    #dems_target <- dems_list[[i]]
    
    fa_twi_name <- paste0(args[4], cluster_huc_name, "_TWI_Facc.tif")
    
    if(!file.exists(fa_twi_name)){
        fs <- dem |>
            terra::rast() |>
            terra::project("EPSG:6347", res = 1) |> 
            terra::terrain(v = c("flowdir", "slope"), unit = "radians")
        fa <- terra::flowAccumulation(fs["flowdir"])
        
        twi <- log(fa/tan(fs["slope"]))
        
        writeRaster(c(fa, twi), fa_twi_name,
                    overwrite = TRUE, names = c("flowacc", "twi"))
    } else {
        print(paste0("TWI and Flow Accum. already made: ", cluster_huc_name))
    }
    
    
}


if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)


future_lapply(list_of_huc_hydro_dems, hydro_func, future.seed = TRUE)


