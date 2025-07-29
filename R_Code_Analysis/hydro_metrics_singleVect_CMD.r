#!/usr/bin/env Rscript

args = c("Data/DEMs/",
         "Data/NY_HUCS/NY_HUCS_08_6350_Cluster.gpkg",
         "Data/TerrainProcessed/",
         11,
         )
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "DEM folder:", args[1], "\n", 
    "Path to a vector study area", args[2], "\n", 
    "Save folder:", args[3], "\n", 
    "region cluster number:", args[4], "\n")

library(terra)
library(MultiscaleDTM)
library(future)
library(future.apply)
suppressPackageStartupMessages(library(tidyterra))
terraOptions(memfrac = 0.25,
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp"
             )
#plan(multisession)

#A vrt for all DEM tiles across NYS to be extracted with a HUC/Study area 
dems <- terra::vrt(list.files(args[1],
                              pattern = ".img$|.tif$",
                              full.names = TRUE))

vector_target <- vect(args[2]) |> 
  terra::project(crs(dems)) |>
  tidyterra::filter(CLUSTER_ID == args[4])
huc_list <- seq_along(vector_target$huc12)[3:5]

dems_target <- terra::crop(dems, vector_target)

hydro_func <- function(huc_in_cluster){
  t <- vector_target$huc12[huc_in_cluster]
  dems_huc <- terra::crop(dems, vector_target[huc_in_cluster])
  slp_fdir <- terra::terrain(dems_huc, v = c("flowdir", "slope"), unit = "radians")
  facc <- terra::flowAccumulation(slp_fdir["flowdir"])
  twi <- log(facc/tan(slp_fdir["slope"]))
  cmb <- c(facc, twi)
  writeRaster(cmb, filename = paste0(args[3], "_", t, "facc_twi_", ".tif"),
              overwrite = TRUE)
  #return(list(facc,twi))
}

#future_lapply(huc_list, hydro_func, future.globals = "vector_target")
lapply(huc_list, hydro_func)
