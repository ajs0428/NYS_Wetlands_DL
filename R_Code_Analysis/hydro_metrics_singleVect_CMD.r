#!/usr/bin/env Rscript

###################
# This script creates a "hydro-conditioned" DTM for hydrologic modeling
    # The new DTMs are named with 'wbt' for Whitebox Tools
# It also creates the hydrologic metrics for Topographic Wetness Index
###################

args = c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
         123,
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
library(dplyr)
library(tidyr)
library(future)
library(future.apply)
library(stringr)
library(flowdem)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_DL/Data/tmp")
print(tempdir())

###############################################################################################

# This takes the vector file of all HUC watersheds, projects them, and filters for the cluster 
# of interest.
# cluster_target is all the HUCs in a cluster
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() 

###############################################################################################
# All the DEMs in a cluster
list_of_huc_dems <- list.files(args[3], 
                               full.names = TRUE, 
                               glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*.tif$"))
                               )
list_of_huc_hydro_dems <- list.files("Data/TerrainProcessed/HUC_DEM_Hydro/", 
                               full.names = TRUE, 
                               glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*.tif$"))
)
dem_hucs <- str_extract(list_of_huc_dems, "(?<=huc_)\\d+")
wbt_dem_hucs <- str_extract(list_of_huc_hydro_dems, "(?<=huc_)\\d+")

# All the non-hydro-conditioned DEMs
non_wbt_list <- list_of_huc_dems[!dem_hucs %in% wbt_dem_hucs]

#HUCs that haven't been hydroconditioned
writeLines(non_wbt_list) 

hydro_condition_func <- function(dem){
    fn <- paste0("Data/TerrainProcessed/HUC_DEM_Hydro/", tools::file_path_sans_ext(basename(dem)), "_wbt.tif")
    message("Hydro-Conditioning for ", fn)
    # wbt_breach_depressions_least_cost(
    #     dem = dem,
    #     output = paste0(stringr::str_remove(dem, ".tif"), "_wbt.tif"),
    #     dist = 100,
    #     fill = TRUE
    # )
    r <- rast(dem)
    rb <- flowdem::breach(r)
    writeRaster(rb, fn, overwrite = TRUE)
}

if(length(non_wbt_list) != 0){
    print(paste0("Requires Hydro-Conditioning: ", length(non_wbt_list)))
    cond_list <- non_wbt_list[!(non_wbt_hucs %in% wbt_hucs)] # the DEMs that need hydrocondition not in the wbt list
    message(cond_list, appendLF = TRUE)
    lapply(cond_list, hydro_condition_func)
} else {
    print(paste0("Already Hydro-Conditioned Number of Hydro-DEMs: ", length(list_of_huc_hydro_dems)))
}



###############################################################################################

hydro_func <- function(dem){
                
    cluster_huc_name <- tools::file_path_sans_ext(basename(dem))
    
    fa_twi_name <- paste0(args[4], cluster_huc_name, "_TWI_Facc.tif")
    
    if(!file.exists(fa_twi_name)){
        message("New TWI and Flow Acc for ", fa_twi_name)
        dem_rast <- rast(dem)
        fs <- dem_rast |>
            # terra::project("EPSG:6347", res = 1) |>
            terra::terrain(v = c("flowdir", "slope"), unit = "radians")
        fa <- terra::flowAccumulation(fs["flowdir"])

        twi <- log(fa/tan(fs["slope"]))
        twi[is.infinite(twi)] <- NA
        writeRaster(c(fa, twi), fa_twi_name,
                    overwrite = TRUE, names = c("flowacc", "twi"))
    } else {
        message("TWI and Flow Accum. already made: ", fa_twi_name)
    }
    

}

# hydro_func(list_of_huc_hydro_dems[1])

# 
if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 64 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

# print(corenum)
# print(options()$future.globals.maxSize)
# 
future_lapply(list_of_huc_hydro_dems, 
              hydro_func, 
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "flowdem"),
              future.globals = TRUE)

terra::tmpFiles(remove = TRUE)

################################################################################################
# non-parallel
# lapply(list_of_huc_hydro_dems, hydro_func)

# r <- rast(list_of_huc_hydro_dems[[1]])
# s <- terra::terrain(r, v = "slope")
# w <- terra::unwrap(cluster_target) |> 
#     tidyterra::filter(huc12 == str_extract(list_of_huc_hydro_dems[[1]], "(?<=huc_)\\d+")) |> 
#     terra::crop(x = vect("Data/Hydrography/NHD_NYS_wb_area.gpkg")) |> 
#     terra::mask(x = s, inverse = TRUE, updatevalue = -999)
# 
# cd <- costDist(w, target = -999, maxiter = 1)
