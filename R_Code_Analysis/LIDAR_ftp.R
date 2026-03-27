library(curl)
library(stringr)
library(sf)
library(dplyr)
library(lidR)
library(terra)
library(future)
library(future.apply)

nys_lidar_ftp <- "ftp://ftp.gis.ny.gov/elevation/LIDAR/"

# List FTP directory contents
list_ftp_dir <- function(ftp_url) {
    con <- curl(ftp_url, open = "r")
    on.exit(close(con))
    
    lines <- readLines(con)
    names <- str_extract(lines, "[^\\s]+$")
    is_dir <- str_detect(lines, "^d")
    
    tibble(
        name = names,
        is_directory = is_dir,
        full_path = paste0(ftp_url, names, ifelse(is_dir, "/", ""))
    )
}

# Download file to temp or specified location
download_ftp_file <- function(ftp_url, dest_dir = tempdir()) {
    filename <- basename(ftp_url)
    dest_path <- file.path(dest_dir, filename)
    curl_download(ftp_url, dest_path, quiet = TRUE)
    dest_path
}

### Find tiles overlapping a set of HUC12 boundaries
# index_path: path to a local tile index GPKG (from download_lidar_indexes.R)
# huc12_sf: sf object with one or more HUC12 polygons
get_overlapping_tiles <- function(index_path, huc12_sf) {

    tile_index <- st_read(index_path, quiet = TRUE)

    # Transform HUC12s to match tile index CRS
    huc12_transformed <- st_transform(huc12_sf, st_crs(tile_index))

    # Find tiles intersecting ANY of the HUC12 polygons
    intersects_mat <- st_intersects(tile_index, huc12_transformed, sparse = FALSE)
    hits <- apply(intersects_mat, 1, any)
    overlapping <- tile_index[hits, ]

    if (nrow(overlapping) == 0) {
        warning("No overlapping tiles found in: ", index_path)
        return(NULL)
    }

    # Build tile_name and ftp_url from DIRECT_DL
    # DIRECT_DL contains full HTTPS path including subdirectories
    # Convert to FTP: https://gisdata.ny.gov/ → ftp://ftp.gis.ny.gov/
    overlapping |>
        mutate(
            tile_name = as.character(FILENAME),
            ftp_url = str_replace(DIRECT_DL,
                                  "https://gisdata.ny.gov/",
                                  "ftp://ftp.gis.ny.gov/")
        )
}

# Per-pixel vegetation metrics function (top-level for lidR formula scoping)
veg_metrics <- function(z, i) {
    n <- length(z)
    p95 <- quantile(z, 0.95)
    list(
        mean_intensity = mean(i),
        pct_below_0.5m = sum(z < 0.5) / n,
        pct_0.5_to_2m  = sum(z >= 0.5 & z < 2) / n,
        pct_2m_to_p95  = sum(z >= 2 & z <= p95) / n
    )
}

### Compute lidar vegetation metrics for a single LAS tile
# Returns 4-band raster at 1m resolution in EPSG:6347:
#   Band 1: mean_intensity  — mean return intensity
#   Band 2: pct_below_0.5m  — proportion of returns below 0.5m
#   Band 3: pct_0.5_to_2m   — proportion of returns between 0.5m and 2m
#   Band 4: pct_2m_to_p95   — proportion of returns between 2m and 95th percentile height
compute_lidar_metrics <- function(las_path, out_dir, res = 1) {

    las <- readLAS(las_path, filter = "-drop_withheld -drop_class 7 18")

    # Height-normalize using ground points (class 2) via TIN interpolation
    las <- normalize_height(las, tin())

    # Drop points with negative normalized heights (below-ground noise)
    las <- filter_poi(las, Z >= 0)

    metrics <- pixel_metrics(las, ~veg_metrics(z = Z, i = Intensity), res = res)

    # Reproject to EPSG:6347 if needed
    target_crs <- "EPSG:6347"
    if (!same.crs(crs(metrics), target_crs)) {
        message("  Reprojecting from ", crs(metrics, describe = TRUE)$code, " to EPSG:6347")
        metrics <- project(metrics, target_crs, method = "bilinear", res = res)
    }

    # Fill interior NA holes with 3x3 mean focal filter (edges unchanged)
    # Create a mask of valid pixels before filling so we don't expand the raster footprint
    valid_mask <- !is.na(metrics[[1]])
    valid_mask <- focal(valid_mask, w = matrix(1, 3, 3), fun = "mean")
    for (i in seq_len(nlyr(metrics))) {
        metrics[[i]] <- focal(metrics[[i]], w = matrix(1, 3, 3),
                              fun = "mean", na.rm = TRUE, na.policy = "only")
    }
    # Mask back to original footprint so edges don't expand
    metrics <- mask(metrics, valid_mask, maskvalues = 0)
    # Min-max normalize intensity to 0-1 (raw values vary across sensors/projects)
    int_vals <- values(metrics[[1]], na.rm = TRUE)
    int_min <- min(int_vals)
    int_max <- max(int_vals)
    if (int_max > int_min) {
        metrics[[1]] <- (metrics[[1]] - int_min) / (int_max - int_min)
    } else {
        metrics[[1]] <- metrics[[1]] * 0  # constant value → set to 0
    }
    metrics[[1]] <- ifel(is.na(metrics[[1]]), 0, metrics[[1]]) # makes NA which is usually water 0 intensity
    metrics[[2]] <- ifel(is.na(metrics[[2]]), 1, metrics[[2]]) # makes NA which is usually water 100% below 0.5m
    metrics[[3]] <- ifel(is.na(metrics[[3]]), 0, metrics[[3]]) # makes NA which is usually water 0% between 0.5 and 2m
    metrics[[4]] <- ifel(is.na(metrics[[4]]), 0, metrics[[4]]) # makes NA which is usually water 0% above 2m
    set.names(metrics, c("mean_intensity", "pct_below_0.5m", "pct_0.5_to_2m", "pct_2m_to_p95"))
    # Write multi-band GeoTIFF
    tile_name <- tools::file_path_sans_ext(basename(las_path))
    out_path <- file.path(out_dir, paste0(tile_name, "_metrics.tif"))
    writeRaster(metrics, out_path, overwrite = TRUE)

    cat("Wrote:", out_path, "\n")
    out_path
}

### Process a single tile: download → compute metrics → clean up
process_tile <- function(tile_name, tile_url, out_dir) {
    out_path <- file.path(out_dir, paste0(tools::file_path_sans_ext(tile_name), "_metrics.tif"))

    # Skip if already processed
    if (file.exists(out_path)) {
        message("[", Sys.getpid(), "] Skipping (exists): ", tile_name)
        return(out_path)
    }

    # Each worker gets its own download directory
    dl_dir <- file.path(tempdir(), "lidar_dl")
    dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)

    message("[", Sys.getpid(), "] Downloading: ", tile_name)
    las_path <- tryCatch(
        download_ftp_file(tile_url, dl_dir),
        error = function(e) {
            warning("Failed to download ", tile_name, ": ", e$message)
            return(NULL)
        }
    )
    if (is.null(las_path)) return(NULL)

    message("[", Sys.getpid(), "] Computing metrics: ", tile_name)
    result <- tryCatch(
        compute_lidar_metrics(las_path, out_dir),
        error = function(e) {
            warning("Failed to process ", tile_name, ": ", e$message)
            return(NULL)
        }
    )

    # Clean up raw LAS to save disk space
    unlink(las_path)
    result
}

###############################################################################
# Command-line execution
# Args: gpkg_path, cluster_number, index_path, output_dir
#
# Example:
#   Rscript R_Code_Analysis/Lidar_ftp.R \
#     "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg" \
#     208 \
#     "Data/Lidar/Indexes/NYS_Central_Finger_Lakes_2020.gpkg" \
#     "Data/Lidar/Metrics"
###############################################################################
args <- c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
              208,
              "Data/Lidar/Indexes/FEMA_2019.gpkg",
              "Data/Lidar/Metrics")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
    stop("Usage: Rscript Lidar_ftp.R <gpkg_path> <cluster> <index_path> <output_dir>")
}

gpkg_path   <- args[1]
cluster_num <- args[2]
index_path  <- args[3]
out_dir     <- args[4]
if (future::availableCores() > 16) {
    n_workers <- 1
} else {
    n_workers <- future::availableCores()
}

message("=== Lidar Metrics Pipeline ===")
message("  GPKG:        ", gpkg_path)
message("  Cluster:     ", cluster_num)
message("  Tile Index:  ", index_path)
message("  Output:      ", out_dir)
message("  Workers:     ", n_workers)

# Filter to all HUC12s in this cluster
cluster_hucs <- st_read(gpkg_path, quiet = TRUE) |>
    filter(cluster == cluster_num)
message("  HUC12s in cluster: ", nrow(cluster_hucs))

# Find overlapping tiles from local index
message("\nFinding overlapping tiles...")
tile_index_info <- get_overlapping_tiles(index_path, cluster_hucs)

if (is.null(tile_index_info) || nrow(tile_index_info) == 0) {
    stop("No overlapping tiles found for cluster ", cluster_num)
}

# Deduplicate tiles and filter out partial/small tiles
min_tile_area <- 2249000  # full tiles are 2250000 m^2
unique_tiles <- tile_index_info |>
    as.data.frame() |>
    distinct(tile_name, .keep_all = TRUE) |>
    filter(SHAPE.AREA >= min_tile_area)
message("Unique full-size tiles to process: ", nrow(unique_tiles))

# Create output directory
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Set up parallel workers (sequential if workers = 1)
if (n_workers > 1) {
    plan(future.callr::callr)
    message("Using ", n_workers, " parallel workers")
} else {
    plan(sequential)
}

# Process all tiles (ftp_url already built by get_overlapping_tiles)
results <- future_lapply(seq_len(nrow(unique_tiles)), function(idx) {
    process_tile(unique_tiles$tile_name[idx], unique_tiles$ftp_url[idx], out_dir)
}, future.seed = NULL)

# Reset to sequential
plan(sequential)

n_success <- sum(!sapply(results, is.null))
message("\n=== Done. ", n_success, "/", nrow(unique_tiles),
        " tiles processed. Metrics written to: ", out_dir, " ===")