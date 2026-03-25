library(curl)
library(stringr)
library(sf)
library(dplyr)

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
    
    
    # List project contents and find tile index shapefile
    contents <- list_ftp_dir(project_url)
    
    # Tile indices are usually .shp files with "index" or "tile" in the name
    shp_files <- contents |> 
        filter(str_detect(name, "\\.shp$"), 
               str_detect(tolower(name), "index|tile"))
    
    if (nrow(shp_files) == 0) {
        warning("No tile index shapefile found in: ", project_url)
        return(NULL)
    }
    
    # Download all shapefile components (.shp, .shx, .dbf, .prj)
    shp_base <- str_remove(shp_files$name[1], "\\.shp$")
    shp_extensions <- c(".shp", ".shx", ".dbf", ".prj")
    
    temp_dir <- tempdir()
    for (ext in shp_extensions) {
        file_url <- paste0(project_url, shp_base, ext)
        tryCatch(
            download_ftp_file(file_url, temp_dir),
            error = function(e) NULL
        )
    }
    
    # Read tile index
    tile_index <- st_read(file.path(temp_dir, paste0(shp_base, ".shp")), quiet = TRUE)
    
    # Transform HUC12 to match tile index CRS
    huc12_transformed <- st_transform(huc12_sf, st_crs(tile_index))
    
    # Find intersecting tiles
    overlapping <- tile_index[st_intersects(tile_index, huc12_transformed, sparse = FALSE)[,1], ]
    
    # Look for column containing tile filename/path
    # Common names: "Name", "Tile", "File", "FileName", "LAS_File"
    name_col <- names(overlapping)[str_detect(tolower(names(overlapping)), "name|tile|file")][1]
    
    if (is.na(name_col)) {
        warning("Could not identify tile name column. Columns: ", paste(names(overlapping), collapse = ", "))
        return(overlapping)
    }
    
    overlapping |> 
        mutate(tile_name = as.character(.data[[name_col]]))
}

# Example usage:
huc12 <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg") |> filter(huc12 == "041402011002")
project_url <- "ftp://ftp.gis.ny.gov/elevation/LIDAR/NYSGPO_CentralFingerLakes_2020/"
 
overlapping_tiles <- get_overlapping_tiles(project_url, huc12)
# 
# # Build download URLs and fetch
# laz_urls <- paste0(project_url, overlapping_tiles$tile_name, ".laz")
# walk(laz_urls, download_ftp_file, dest_dir = "data/lidar")