#!/usr/bin/env Rscript

#################
# This script checks the predictor layer rasters before 
# they are used in data extraction, modeling, and prediction
#################

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
    120,
    "Data/NAIP/HUC_NAIP_Processed/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to the HUC Terrain Metrics:", args[1], "\n",
    "- cluster number:", args[2],
    "- path to rasters:", args[3]
)

###############################################################################################
library(terra)
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))


terraOptions(tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
print(tempdir())

###############################################################################################
cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) 
cluster_crs <- st_crs(cluster_target)
###############################################################################################

rast_list <- list.files(path = args[3], pattern = paste0("cluster_", args[2], "_", ".*\\.tif"), full.names = TRUE)
dem_list <- list.files(path = "Data/TerrainProcessed/HUC_DEMs/", pattern = paste0("cluster_", args[2], "_", ".+\\d+\\.tif$"), full.names = TRUE)



check_layers_func <- function(i, cluster_target, rast_list, dem_list){
    cluster_huc_name <- cluster_target$huc12[[i]]
    print(paste0("Cluster:", args[2], " HUC: ",cluster_huc_name))
    
    r_list_crs_ext_res <- list() # Not CRS 6347, different extent, different resolution
    r_list_ext_res <- list() # same CRS, different extent, different resolution
    r_list_res <- list() # same CRS, same extent, different resolution
    r_list <- list() # same CRS, same extent, same resolution
    
    tryCatch({
        r_filename <- rast_list[str_detect(rast_list, cluster_huc_name)]
        print(r_filename)
        dem_filename <- dem_list[str_detect(dem_list, cluster_huc_name)]
        print(dem_filename)
        ri <- rast(r_filename)
        di <- rast(dem_filename)
        r_ext <- ext(ri) # extent 
        r_res <- res(ri)[1] # resolution
        d_ext <- ext(di) # dem extent
        d_res <- res(di)[1] #dem resolution
    

    # Check CRS, Extent, and Resolution against DEM
    if(crs(ri) != crs(di) & r_ext != d_ext & r_res != d_res){
        print("Not CRS 6347, different extent, different resolution")
        r_list_crs_ext_res[[i]] <- r_filename
        r_list_ext_res[[i]] <- r_filename
        r_list_res[[i]] <- r_filename
        r_list[[i]] <- "None"
    } else if(crs(ri) != crs(di) & r_ext != d_ext & r_res == d_res){
        print("Not CRS 6347, different extent, same resolution")
        r_list_crs_ext_res[[i]] <- r_filename
        r_list_ext_res[[i]] <- r_filename
        r_list_res[[i]] <- "None"
        r_list[[i]] <- "None"
    } else if(crs(ri) != crs(di) & r_ext == d_ext & r_res == d_res){
        print("same CRS, different extent, same resolution")
        r_list_crs_ext_res[[i]] <- r_filename
        r_list_ext_res[[i]] <- "None"
        r_list_res[[i]] <- "None"
        r_list[[i]] <- "None"
    } else if(crs(ri) == crs(di) & r_ext != d_ext & r_res == d_res){
        print("same CRS, different extent, same resolution")
        r_list_crs_ext_res[[i]] <- "None"
        r_list_ext_res[[i]] <- r_filename
        r_list_res[[i]] <- "None"
        r_list[[i]] <- "None"
    } else if(crs(ri) == crs(di) & r_ext != d_ext & r_res != d_res){
        print("same CRS, different extent, different resolution")
        r_list_crs_ext_res[[i]] <- "None"
        r_list_ext_res[[i]] <- r_filename
        r_list_res[[i]] <- r_filename
        r_list[[i]] <- "None"
    } else if(crs(ri) == crs(di) & r_ext == d_ext & r_res != d_res){
        print("same CRS, same extent, different resolution")
        r_list_crs_ext_res[[i]] <- "None"
        r_list_ext_res[[i]] <- "None"
        r_list_res[[i]] <- r_filename
        r_list[[i]] <- "None"
    } else if(crs(ri) == crs(di) & r_ext == d_ext & r_res == d_res){
        print("same CRS, same extent, same resolution")
        r_list_crs_ext_res[[i]] <- "None"
        r_list_ext_res[[i]] <- "None"
        r_list_res[[i]] <- "None"
        r_list[[i]] <- r_filename
    } else {
        r_list_crs_ext_res[[i]] <- paste0("Something", cluster_huc_name)
        r_list_ext_res[[i]] <- paste0("Something", cluster_huc_name)
        r_list_res[[i]] <- paste0("Something", cluster_huc_name)
        r_list[[i]] <- paste0("Something", cluster_huc_name)
    }
    
    return(list("r_list_crs_ext_res" = unlist(r_list_crs_ext_res), 
                "r_list_ext_res" = unlist(r_list_ext_res), 
                "r_list_res" = unlist(r_list_res), 
                "r_list" = unlist(r_list)))
        
    }, error = function(msg){
        print(paste("Error occurred:", msg$message))
        r_list_crs_ext_res[[i]] <- paste0("RastError: ", cluster_huc_name)
        r_list_ext_res[[i]] <- paste0("RastError: ", cluster_huc_name)
        r_list_res[[i]] <- paste0("RastError: ", cluster_huc_name)
        r_list[[i]] <- paste0("RastError: ", cluster_huc_name)
        
        return(list("r_list_crs_ext_res" = unlist(r_list_crs_ext_res), 
                    "r_list_ext_res" = unlist(r_list_ext_res), 
                    "r_list_res" = unlist(r_list_res), 
                    "r_list" = unlist(r_list)))
    })
}

lists <- lapply(
    seq_along(cluster_target$huc12)[3],
    check_layers_func, # function
    cluster_target = cluster_target,
    rast_list = rast_list,
    dem_list = dem_list
)

# corenum <-  4
# options(future.globals.maxSize= 8.0 * 1e9)
# plan(multisession, workers = corenum) 
# 
# print(corenum)
# print(options()$future.globals.maxSize)
# 
# future_lapply(huc_extract, f_list[1:2], v_list[1:2], future.seed = TRUE)


r_df <- if(length(lists) > 0){
    bind_rows(lists) |> 
        dplyr::mutate(cluster = paste0("cluster_", args[2])) |> 
        dplyr::select(cluster, everything())
} else {
    tibble(
        cluster = paste0("cluster_", args[2]),
        r_list_crs_ext_res = "missing", 
        r_list_ext_res = "missing", 
        r_list_res = "missing", 
        r_list = "missing"
    )
}
r_df

write.csv(r_df, paste0("Data/Dataframes/RasterChecks/cluster_", args[2], "_", str_extract(args[3], "NAIP|CHM|DEM|Hydro|TerrainMetrics"), "_raster_checklist.csv"))
