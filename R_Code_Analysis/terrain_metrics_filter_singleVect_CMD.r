#!/usr/bin/env Rscript

args = c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
         120,
         "Data/TerrainProcessed/HUC_DEMs/",
         "slp",
         "Data/TerrainProcessed/HUC_TerrainMetrics/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Metric (slp, dmv, curv):", args[4], "\n", 
    "- Path to the Save folder", args[5], "\n"
)
###############################################################################################

library(terra)
library(sf)
library(MultiscaleDTM)
library(future)
library(future.apply)
library(future.callr)
library(stringr)

# Configure terra for efficiency
terraOptions(
    tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp",
    memfrac = 0.6,      # Use up to 60% of RAM before writing to disk
    threads = 2         # Internal threading for terra operations (per worker)
)
###############################################################################################

process_scale <- function(dem_rast, scale_factor, output_file, metric, scale_label) {
    
    if (file.exists(output_file)) {
        message(paste0(metric, " ", scale_label, " already exists, skipping"))
        return(invisible(NULL))
    }
    
    message(paste0("Creating ", metric, " ", scale_label, " for: ", output_file))
    
    # Aggregate and resample - store intermediate to avoid re-computation
    smoothed <- dem_rast |>
        terra::aggregate(scale_factor, fun = "mean", na.rm = TRUE) |>
        terra::resample(y = dem_rast, method = "cubicspline")
    
    # Compute metric and write directly to file
    result <- switch(metric,
                     "slp" = terra::terrain(
                         smoothed,
                         v = c("slope", "aspect", "TPI", "TRI"),
                         filename = output_file,
                         overwrite = TRUE
                     ),
                     "dmv" = {
                         dmv_result <- MultiscaleDTM::DMV(smoothed, w = c(3, 3), 
                                                          stand = "none", include_scale = FALSE)
                         writeRaster(dmv_result, output_file, overwrite = TRUE)
                     },
                     "curv" = {
                         curv_result <- MultiscaleDTM::Qfit(smoothed, w = c(3, 3), include_scale = TRUE,
                                                            metrics = c("meanc", "planc", "profc"))
                         writeRaster(curv_result, output_file, overwrite = TRUE)
                     },
                     stop("Unknown Metric")
    )
    
    # Explicit cleanup of intermediate
    rm(smoothed, result)
    gc(verbose = FALSE)
    
    return(invisible(NULL))
}


###############################################################################################

terrain_function <- function(dem_path, metric) {
    
    cluster_huc_name <- str_remove(basename(dem_path), "\\.tif$")
    message(paste0("\n=== Processing: ", cluster_huc_name, " ==="))
    
    # Define output paths
    base_path <- paste0(args[5], cluster_huc_name, "_terrain_", metric)
    output_files <- list(
        "5m"   = paste0(base_path, "_5m.tif"),
        "100m" = paste0(base_path, "_100m.tif"),
        "500m" = paste0(base_path, "_500m.tif")
    )
    
    # Load DEM
    dem_rast <- rast(dem_path)
    
    tryCatch({
        # Process each scale - DEM loaded only once
        process_scale(dem_rast, 5,   output_files[["5m"]],   metric, "5m")
        process_scale(dem_rast, 100, output_files[["100m"]], metric, "100m")
        process_scale(dem_rast, 500, output_files[["500m"]], metric, "500m")
        
    }, error = function(e) {
        message(paste0("ERROR at: ", cluster_huc_name, " - ", e$message))
        return(NA)
    })
    
    # Cleanup
    rm(dem_rast)
    gc(verbose = FALSE)
    tmpFiles(remove = TRUE)
    
    return(invisible(NULL))
}
###############################################################################################

list_of_huc_dems <- list.files(
    args[3],
    pattern = paste0("^cluster_", args[2], "_.*\\.tif$"),  
    full.names = TRUE
) |> str_subset(pattern = "wbt", negate = TRUE)

message(paste0("Found ", length(list_of_huc_dems), " DEMs to process"))

###############################################################################################
# list_of_huc_dems <- list.files(args[3], full.names = TRUE, glob2rx(pattern = paste0("^cluster_", args[2], "_", "*\\*.tif$"))) |> 
#     stringr::str_subset(pattern = "wbt", negate = TRUE)
# 
# terrain_function <- function(dems_target, metric = args[4]){
# 
#     cluster_huc_name <- stringr::str_remove(basename(dems_target), ".tif")
#     print(cluster_huc_name)
#     
#     file_5m <- paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_5m.tif")
#     file_100m <- paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_100m.tif")
#     file_500m <- paste0(args[5], cluster_huc_name, "_terrain_", args[4], "_500m.tif")
#     
#     dem_rast <- rast(dems_target)
#     
#     tryCatch({
#     if(stringr::str_detect(metric, "slp")){
#         if(!file.exists(file_5m)){
#             print(paste0("Creating slp 5m for: ", file_5m))
#             dem_rast |>
#                 terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
#                                filename = file_5m,
#                                overwrite = TRUE, names = c("slope_5m", "aspect_5m", "TPI_5m", "TRI_5m"))
#             gc(verbose = FALSE)
#         } else {print(paste0(args[4], " 5m Metric files accounted for"))}
#         if(!file.exists(file_100m)){
#             print(paste0("Creating slp 100m for: ", file_100m))
#             dem_rast |> 
#                 terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
#                                filename = file_100m,
#                                overwrite = TRUE, names = c("slope_100m", "aspect_100m", "TPI_100m", "TRI_100m"))
#             gc(verbose = FALSE)
#         }else {print(paste0(args[4], " 100m Metric files accounted for"))}
#         if(!file.exists(file_500m)){
#             print(paste0("Creating slp 500m for: ", file_500m))
#             dem_rast |>
#                 terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 terra::terrain(v = c("slope", "aspect", "TPI", "TRI"),
#                                filename = file_500m,
#                                overwrite = TRUE, names = c("slope_500m", "aspect_500m", "TPI_500m", "TRI_500m"))
#             gc(verbose = FALSE)
#         } else {print(paste0(args[4], " 500m Metric files accounted for"))}
#         
#     } else if (stringr::str_detect(metric, "dmv")){
#         if(!file.exists(file_5m)){
#             print(paste0("Creating dmv 5m for: ", file_5m))
#             dem_rast |> 
#                 terra::aggregate(5, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
#                                    include_scale = FALSE) |>
#                 # terra::wrap()
#                 writeRaster(file_5m,
#                             overwrite = TRUE, names = c("dmv_5m"))
#             gc(verbose = FALSE)
#         } else {print(paste0(args[4], " 5m Metric files accounted for"))}
#         if(!file.exists(file_100m)){
#             print(paste0("Creating dmv 100m for: ", file_100m))
#             dem_rast |> 
#                 terra::aggregate(100, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
#                                    include_scale = FALSE) |>
#                 # terra::wrap()
#                 writeRaster(file_100m,
#                             overwrite = TRUE, names = c("dmv_100m"))
#             gc(verbose = FALSE)
#         }else {print(paste0(args[4], " 100m Metric files accounted for"))}
#         if(!file.exists(file_500m)){
#             print(paste0("Creating dmv 500m for: ", file_500m))
#             dem_rast |> 
#                 terra::aggregate(500, fun = "mean", na.rm = TRUE) |> # aggregate first
#                 terra::resample(y = dem_rast, method = "cubicspline") |> # resample to original resolution
#                 MultiscaleDTM::DMV(w = c(3,3), stand = "none", # I think "none" so that NA won't be produced
#                                    include_scale = FALSE) |>
#                 # terra::wrap()
#                 writeRaster(file_500m,
#                             overwrite = TRUE, names = c("dmv_500m"))
#             gc(verbose = FALSE)
#         }else {print(paste0(args[4], " 500m Metric files accounted for"))}
#         
#     } else if(stringr::str_detect(metric, "curv")){
#         if(!file.exists(file_5m)){
#             print(paste0("Creating curv 5m for: ", file_5m))
#             dem_rast |> 
#                 terra::aggregate(5, fun = "mean", na.rm = TRUE) |> 
#                 terra::resample(y = dem_rast, method = "cubicspline") |> 
#                 MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>
# 
#                 writeRaster(file_5m,
#                             overwrite = TRUE, names = c("meanc_5m", "planc_5m", "profc_5m"))
#             gc(verbose = FALSE)
#         } else {print(paste0(args[4], " 5m Metric files accounted for"))}
#         if(!file.exists(file_100m)){
#             print(paste0("Creating curv 100m for: ", file_100m))
#             dem_rast |>
#                 terra::aggregate(100, fun = "mean", na.rm = TRUE) |>
#                 terra::resample(y = dem_rast, method = "cubicspline") |> 
#                 MultiscaleDTM::Qfit(w = c(3,3), include_scale = TRUE, metrics = c("meanc", "planc", "profc"))|>
# 
#             writeRaster(file_100m,
#                         overwrite = TRUE, names = c("meanc_100m", "planc_100m", "profc_100m"))
#             gc(verbose = FALSE)
#         }else {print(paste0(args[4], " 100m Metric files accounted for"))}
#         if(!file.exists(file_500m)){
#             print(paste0("Creating curv 500m for: ", file_500m))
#             dem_rast |>
#                 terra::aggregate(500, fun = "mean", na.rm = TRUE) |>
#                 terra::resample(y = dem_rast, method = "cubicspline") |> 
#                 MultiscaleDTM::Qfit(w = c(3,3),include_scale = TRUE, metrics = c("meanc", "planc", "profc")) |>
# 
#             writeRaster(file_500m,
#                         overwrite = TRUE, names = c("meanc_500m", "planc_500m", "profc_500m"))
#             gc(verbose = FALSE)
#         } else {print(paste0(args[4], " 500m Metric files accounted for"))}
#     } else {
#         print("No terrain metric specified or not identified") 
#     }
#     }, error = function(msg){
#         message(paste0("Error at: ", cluster_huc_name))
#         return(NA)
#     }
#     )
#     gc(verbose = FALSE)
#     tmpFiles(remove = TRUE)
# }


if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr)

future_lapply(
    list_of_huc_dems,
    terrain_function,
    metric = args[4],
    future.seed = TRUE,
    future.scheduling = 1.0  # Dynamic load balancing
)

# lapply(list_of_huc_dems, terrain_function, cluster_target)
# 
# r <- rast("Data/TerrainProcessed/HUC_DEMs/cluster_120_huc_020200060609.tif")
# 
# rac <- r |> terra::aggregate(500, fun = "mean", na.rm = TRUE) |>
#     terra::resample(y = (r), method = "cubicspline") |> 
#     MultiscaleDTM::Qfit( w = c(3,3),include_scale = TRUE, metrics = c("meanc", "planc", "profc"))
