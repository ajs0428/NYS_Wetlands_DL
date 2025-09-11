#!/usr/bin/env Rscript

args = c(
    208,
    "coarse",
    "Data/Predicted_Wetland_Rasters/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Cluster number (integer 1-200ish):", args[1], "\n",
    "- Model Type (coarse for binary, multi for wetland classes):", args[2], "\n",
    "- Path to Export:", args[3], "\n"
)

###############################################################################################

library(terra)
library(sf)
library(here)
library(future)
library(future.apply)
library(collapse)
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(tidyverse))
library(tidymodels)
library(bundle)
library(vip)

terraOptions(memfrac = 0.40,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")


###############################################################################################

## Read Data

predictor_stacks <- list.files("Data/Predictor_Stacks/", full.names = TRUE, pattern = paste0("cluster_", args[1], ".*\\.tif"))
model <- list.files("Data/Models/", full.names = TRUE, pattern = paste0("Cluster_", args[1],"_",args[2], ".*\\.rds"), ignore.case = TRUE)



###############################################################################################

pred_function <- function(pred_stack, mod){
    mod_name <- str_remove(basename(mod),  "\\.[^.]*$")
    huc_name <- str_extract(basename(pred_stack), "huc_[0-9]+")
    
    print(here(paste0(args[3], mod_name, "_", huc_name, ".tif")))

    terra::predict(
        pred_stack |> rast(),
        mod |> readRDS() |> unbundle(),
        type = "prob",
        na.rm = TRUE,
        filename = here(paste0(args[3], mod_name, huc_name, ".tif")),
        overwrite = TRUE
    )
}

plan(multisession, workers = 2)
#options(future.globals.maxSize= 1.0 * 1e9)
future_lapply(predictor_stacks, pred_function, mod = model, future.seed = TRUE, future.packages = c("tidymodels", "here", "terra"))
# lapply(predictor_stacks[[5]], pred_function, mod = model)
# test <- pred_function(pred_stack = predictor_stacks[1], mod = model)
# 
# 
# huc_name <- "041402011009"
# dem_list <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".*\\d+.tif", full.names = TRUE)
# terr_list <- list.files(path = "Data/TerrainProcessed/HUC_TerrainMetrics/", pattern = paste0("cluster_", args[2], ".*\\m.tif"), full.names = TRUE) %>% 
#     .[!str_detect(., "1000m")]
# hydro_list <- list.files(path = "Data/TerrainProcessed/HUC_Hydro//", pattern = paste0("cluster_", args[2], ".*\\.tif"), full.names = TRUE)
# naip_huc_list <- list.files(path = "Data/NAIP/NAIP_HUC_Merged/", pattern = paste0(".*\\cluster_", args[2], ".*\\.tif"), full.names = TRUE)
# 
# 
# tr <- terr_list[str_detect(terr_list, huc_name)] |> rast()
# hr <- hydro_list[str_detect(hydro_list, huc_name)] |> rast()
# nr <- naip_huc_list[str_detect(naip_huc_list, huc_name)] |> rast() |> 
#     terra::resample(tr, method = "bilinear", threads = TRUE)
# dr <- dem_list[str_detect(dem_list, huc_name)] |> rast() |>
#     terra::project(crs(tr))
# set.names(dr, "DEM")
# # print(ext(tr))
# # print(ext(hr))
# # print(ext(nr))
# cr <- c(dr, tr, hr, nr)
# writeRaster(cr,
#             filename = paste0("Data/Predictor_Stacks/cluster_",
#                               args[2], "_huc_", huc_name, "_raster_predictors.tif"),
#             overwrite = TRUE)
# 
# mod_name <- str_remove(basename(model[[1]]),  "\\.[^.]*$")
# huc_name <- str_extract(basename(predictor_stacks[[5]]), "huc_[0-9]+")
# r <- rast("Data/Predictor_Stacks//cluster_208_huc_041402011009_raster_predictors.tif")
# terra::predict(r,
#                model[[1]]|> readRDS() |> unbundle(), 
#                filename = here(paste0(args[3], mod_name, huc_name, ".tif")),
#                overwrite = TRUE,
#                type = "prob", na.rm = TRUE)
# 
