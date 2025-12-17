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
                         overwrite = TRUE, 
                         names = c(paste0("slope_", scale_label),
                                   paste0("aspect_", scale_label),
                                   paste0("TPI_", scale_label),
                                   paste0("TRI_", scale_label))
                     ),
                     "dmv" = {
                         dmv_result <- MultiscaleDTM::DMV(smoothed, w = c(3, 3), 
                                                          stand = "none", include_scale = FALSE)
                         writeRaster(dmv_result, output_file, overwrite = TRUE,
                                     names = c(paste0("dmv_", scale_label)))
                     },
                     "curv" = {
                         curv_result <- MultiscaleDTM::Qfit(smoothed, w = c(3, 3), include_scale = TRUE,
                                                            metrics = c("meanc", "planc", "profc"))
                         writeRaster(curv_result, output_file, overwrite = TRUE,
                                     names = c(paste0("meanc_", scale_label),
                                               paste0("planc_", scale_label),
                                               paste0("profc_", scale_label)))
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
