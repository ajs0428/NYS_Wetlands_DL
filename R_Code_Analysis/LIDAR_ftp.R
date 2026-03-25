library(curl)
library(stringr)
library(sf)
library(dplyr)
library(lidR)
library(terra)

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

### Find tiles overlapping a HUC12 boundary
get_overlapping_tiles <- function(project_url, huc12_sf) {

    # List project contents and find tile index (zipped or loose shapefile)
    contents <- list_ftp_dir(project_url)

    # Try zipped index first (most common on NYS FTP)
    zip_files <- contents |>
        filter(str_detect(tolower(name), "index|tile"),
               str_detect(name, "\\.zip$"))

    # Fall back to loose shapefiles
    shp_files <- contents |>
        filter(str_detect(name, "\\.shp$"),
               str_detect(tolower(name), "index|tile"))

    temp_dir <- file.path(tempdir(), "tile_index")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

    if (nrow(zip_files) > 0) {
        # Download and extract zip
        zip_path <- download_ftp_file(zip_files$full_path[1], temp_dir)
        unzip(zip_path, exdir = temp_dir)
        shp_path <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
    } else if (nrow(shp_files) > 0) {
        # Download loose shapefile components
        shp_base <- str_remove(shp_files$name[1], "\\.shp$")
        for (ext in c(".shp", ".shx", ".dbf", ".prj", ".cpg")) {
            tryCatch(
                download_ftp_file(paste0(project_url, shp_base, ext), temp_dir),
                error = function(e) NULL
            )
        }
        shp_path <- file.path(temp_dir, paste0(shp_base, ".shp"))
    } else {
        warning("No tile index found in: ", project_url)
        return(NULL)
    }

    # Read tile index
    tile_index <- st_read(shp_path, quiet = TRUE)

    # Transform HUC12 to match tile index CRS
    huc12_transformed <- st_transform(huc12_sf, st_crs(tile_index))

    # Find intersecting tiles
    overlapping <- tile_index[st_intersects(tile_index, huc12_transformed, sparse = FALSE)[,1], ]

    # Use FILENAME column (standard on NYS FTP), fall back to heuristic
    if ("FILENAME" %in% names(overlapping)) {
        name_col <- "FILENAME"
    } else {
        name_col <- names(overlapping)[str_detect(tolower(names(overlapping)), "name|tile|file")][1]
    }

    if (is.na(name_col)) {
        warning("Could not identify tile name column. Columns: ",
                paste(names(overlapping), collapse = ", "))
        return(overlapping)
    }

    overlapping |>
        mutate(tile_name = as.character(.data[[name_col]]))
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

    # Ensure CRS is EPSG:6347
    crs(metrics) <- "EPSG:6347"

    # Write multi-band GeoTIFF
    tile_name <- tools::file_path_sans_ext(basename(las_path))
    out_path <- file.path(out_dir, paste0(tile_name, "_metrics.tif"))
    writeRaster(metrics, out_path, overwrite = TRUE)

    cat("Wrote:", out_path, "\n")
    out_path
}

###############################################################################
# Command-line execution
# Args: gpkg_path, cluster_number, project_url, output_dir
#
# Example:
#   Rscript R_Code_Analysis/Lidar_ftp.R \
#     "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg" \
#     208 \
#     "ftp://ftp.gis.ny.gov/elevation/LIDAR/NYSGPO_CentralFingerLakes_2020/" \
#     "Data/Lidar/Metrics"
###############################################################################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
    stop("Usage: Rscript Lidar_ftp.R <gpkg_path> <cluster> <project_url> <output_dir>")
}

gpkg_path   <- args[1]
cluster_num <- args[2]
project_url <- args[3]
out_dir     <- args[4]

message("=== Lidar Metrics Pipeline ===")
message("  GPKG:        ", gpkg_path)
message("  Cluster:     ", cluster_num)
message("  FTP Project: ", project_url)
message("  Output:      ", out_dir)

# Filter to all HUC12s in this cluster
cluster_hucs <- st_read(gpkg_path, quiet = TRUE) |>
    filter(cluster == cluster_num)
message("  HUC12s in cluster: ", nrow(cluster_hucs))

# Download tile index once for the whole project
message("\nFetching tile index...")
tile_index_info <- get_overlapping_tiles(project_url, cluster_hucs)

if (is.null(tile_index_info) || nrow(tile_index_info) == 0) {
    stop("No overlapping tiles found for cluster ", cluster_num)
}

# Deduplicate tiles (multiple HUC12s may overlap the same tile)
unique_tiles <- tile_index_info |>
    as.data.frame() |>
    distinct(tile_name, .keep_all = TRUE)
message("Unique tiles to process: ", nrow(unique_tiles))

# Build FTP download URLs
tile_urls <- paste0("ftp://ftp.gis.ny.gov/", unique_tiles$FTP_PATH, unique_tiles$tile_name)

# Create output and temp download directories
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dl_dir <- file.path(tempdir(), "lidar_dl")
dir.create(dl_dir, showWarnings = FALSE)

# Process each tile: download → compute metrics → clean up raw file
for (idx in seq_len(nrow(unique_tiles))) {
    tile <- unique_tiles$tile_name[idx]
    out_path <- file.path(out_dir, paste0(tools::file_path_sans_ext(tile), "_metrics.tif"))

    # Skip if already processed
    if (file.exists(out_path)) {
        message("[", idx, "/", nrow(unique_tiles), "] Skipping (exists): ", tile)
        next
    }

    message("[", idx, "/", nrow(unique_tiles), "] Downloading: ", tile)
    las_path <- tryCatch(
        download_ftp_file(tile_urls[idx], dl_dir),
        error = function(e) {
            warning("  Failed to download ", tile, ": ", e$message)
            return(NULL)
        }
    )
    if (is.null(las_path)) next

    message("  Computing metrics...")
    tryCatch(
        compute_lidar_metrics(las_path, out_dir),
        error = function(e) warning("  Failed to process ", tile, ": ", e$message)
    )

    # Clean up raw LAS to save disk space
    unlink(las_path)
}

message("\n=== Done. Metrics written to: ", out_dir, " ===")