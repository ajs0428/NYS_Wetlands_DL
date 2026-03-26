#!/usr/bin/env Rscript
# =============================================================================
# Download all NYS LiDAR tile index layers from ArcGIS REST service
# Source: https://elevation.its.ny.gov/arcgis/rest/services/LAS_Indexes/MapServer/
# =============================================================================

library(sf)
library(dplyr)
library(stringr)
library(jsonlite)

# --- Configuration -----------------------------------------------------------

out_dir <- "Data/Lidar/Indexes"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_url <- "https://elevation.its.ny.gov/arcgis/rest/services/LAS_Indexes/MapServer"
batch_size <- 1000 # MaxRecordCount for this service

# Layer IDs and names (layer 2 is a group layer, skip it)
layers <- tibble::tribble(
    ~id, ~name,
    0L, "USGS_2024",
    1L, "NYS_Lake_Ontario_Shoreline_2023",
    3L, "USGS_Lake_Ontario_Hudson_River_2022",
    4L, "NYS_Southeast_4_County_2022",
    5L, "NYS_Central_Finger_Lakes_2020",
    6L, "NYS_Erie_Genesee_Livingston_2019",
    7L, "FEMA_2019",
    8L, "FEMA_Fulton_Saratoga_Herkimer_Franklin_2017",
    9L, "NYS_Cayuga_Oswego_2018",
    10L, "NYC_Topobathymetric_2017",
    11L, "NYS_Southwest_Fall_2017",
    12L, "NYS_Southwest_Spring_2017",
    13L, "FEMA_Franklin_St_Lawrence_2016_17",
    14L, "FEMA_Oneida_Subbasin_2016_17",
    15L, "NYS_Allegany_Steuben_2016",
    16L, "NYS_Madison_Otsego_2015",
    17L, "NYS_Columbia_Rensselaer_2016",
    18L, "NYS_Warren_Washington_Essex_2015",
    19L, "USGS_Clinton_Essex_Franklin_2014",
    20L, "FEMA_Great_Lakes_2014",
    21L, "USGS_3_County_2014",
    22L, "USGS_Schoharie_Montgomery_2014",
    23L, "USGS_Long_Island_2014",
    24L, "USGS_NYC_2014",
    25L, "NYS_Great_Gully_2014",
    26L, "FEMA_Seneca_Watershed_2012",
    27L, "FEMA_Hudson_Hoosic_2012",
    28L, "USDA_Livingston_2011",
    29L, "USDA_Genesee_2011",
    30L, "USDA_Lewis_2011",
    31L, "USDA_Dean_Creek_2011",
    32L, "USGS_North_East_2011",
    33L, "FEMA_Chemung_Watershed_2011",
    34L, "NYS_Greene_East_Half_2010",
    35L, "NYS_Jefferson_Black_River_2010",
    36L, "NYS_Rensselaer_Hoosick_River_2010",
    37L, "NYCDEP_West_Of_Hudson_2009",
    38L, "NYCDEP_East_Of_Hudson_2009",
    39L, "NYSDEC_Capital_District_2008",
    40L, "NYSDEC_Putnam_2008",
    41L, "County_Tompkins_2008",
    42L, "FEMA_Oneida_2008",
    43L, "County_Erie_2008",
    44L, "FEMA_Delaware_2005_2007",
    45L, "FEMA_Mohawk_2007",
    46L, "County_Niagara_2007",
    47L, "FEMA_Sullivan_2005_2007",
    48L, "FEMA_Susquehanna_Basin_2007",
    49L, "County_Ontario_2006",
    50L, "FEMA_Suffolk_2006",
    51L, "NYSDEC_Ulster_2005",
    52L, "County_Chemung_2005",
    53L, "County_Cortland_2005",
    54L, "NYSDEC_Onondaga_2005",
    55L, "NYSDEC_Dolgeville_2005",
    56L, "County_Chemung_2002",
    57L, "NYSDEC_Onondaga_2002"
)

# --- Helper functions --------------------------------------------------------

get_feature_count <- function(layer_id) {
    url <- paste0(
        base_url, "/", layer_id,
        "/query?where=1%3D1&returnCountOnly=true&f=json"
    )
    resp <- tryCatch(fromJSON(url), error = function(e) NULL)
    if (is.null(resp) || is.null(resp$count)) return(NA_integer_)
    as.integer(resp$count)
}

download_layer <- function(layer_id, layer_name) {
    
    out_file <- file.path(out_dir, paste0(layer_name, ".gpkg"))
    
    if (file.exists(out_file)) {
        message("  Skipping (already exists): ", layer_name)
        return(TRUE)
    }
    
    # Get feature count to determine if pagination is needed
    n_features <- get_feature_count(layer_id)
    if (is.na(n_features)) {
        warning("  Could not get feature count for layer ", layer_id, " (", layer_name, ")")
        return(FALSE)
    }
    message("  Feature count: ", n_features)
    
    if (n_features == 0) {
        message("  No features, skipping.")
        return(TRUE)
    }
    
    # Download — paginate if needed
    if (n_features <= batch_size) {
        url <- paste0(
            base_url, "/", layer_id,
            "/query?where=1%3D1&outFields=*&f=geojson"
        )
        layer_sf <- tryCatch(
            st_read(url, quiet = TRUE),
            error = function(e) {
                warning("  Failed to read layer ", layer_id, ": ", e$message)
                NULL
            }
        )
    } else {
        message("  Paginating (", n_features, " features)...")
        batches <- list()
        offset <- 0
        
        repeat {
            url <- paste0(
                base_url, "/", layer_id,
                "/query?where=1%3D1&outFields=*&f=geojson",
                "&resultOffset=", offset,
                "&resultRecordCount=", batch_size
            )
            batch <- tryCatch(st_read(url, quiet = TRUE), error = function(e) NULL)
            if (is.null(batch) || nrow(batch) == 0) break
            batches <- c(batches, list(batch))
            message("    Fetched ", offset + nrow(batch), " / ", n_features)
            offset <- offset + batch_size
            if (offset >= n_features) break
        }
        
        layer_sf <- if (length(batches) > 0) bind_rows(batches) else NULL
    }
    
    if (is.null(layer_sf) || nrow(layer_sf) == 0) {
        warning("  No data returned for layer ", layer_id, " (", layer_name, ")")
        return(FALSE)
    }
    
    # Write to GeoJSON
    st_write(layer_sf, out_file, driver = "GPKG", quiet = TRUE)
    message("  Saved: ", out_file, " (", nrow(layer_sf), " features)")
    return(TRUE)
}

# --- Main loop ---------------------------------------------------------------

message("Downloading ", nrow(layers), " LiDAR tile index layers to: ", out_dir)
message(str_dup("=", 60))

results <- layers |>
    mutate(success = mapply(function(id, name) {
        message("\n[", id, "] ", name)
        download_layer(id, name)
    }, id, name))

# --- Summary -----------------------------------------------------------------

message("\n", str_dup("=", 60))
message("Done. ", sum(results$success), " / ", nrow(results), " layers downloaded.")

if (any(!results$success)) {
    message("\nFailed layers:")
    results |>
        filter(!success) |>
        mutate(msg = paste0("  [", id, "] ", name)) |>
        pull(msg) |>
        walk(message)
}