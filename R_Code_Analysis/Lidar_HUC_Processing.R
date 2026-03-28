args <- c("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg",
          208,
          "Data/Lidar/HUC_Lidar_Metrics/")

args <- commandArgs(trailingOnly = TRUE)

(message("these are the arguments: \n", 
         "- Path to cluster of HUCs files: ", args[1], "\n",
         "- Cluster to select: ", args[2], "\n",
         "- Output path for Lidar HUC metrics files:", args[3], "\n"
))

gpkg_path   <- args[1]
cluster_num <- args[2]
out_dir     <- args[3]

library(curl)
library(stringr)
library(sf)
library(dplyr)
library(lidR)
library(terra)


message("=== Lidar Metrics Pipeline ===")
message("  GPKG:        ", gpkg_path)
message("  Cluster:     ", cluster_num)
message("  Output:      ", out_dir)

# Filter to all HUC12s in this cluster
cluster_hucs <- st_read(gpkg_path, quiet = TRUE) |>
    filter(cluster == cluster_num)
message(" HUC12s in cluster: ", nrow(cluster_hucs))
huc_numbers<- cluster_hucs$huc12

current_lidar_metrics <- list.files("Data/Lidar/Metrics", full.names = TRUE)
current_lidar_metrics_fn <- sub("_metrics.tif", "", basename(current_lidar_metrics) )
message("Length of current lidar metrics: ",length(current_lidar_metrics))
# For each HUC 
    # Find the overlapping indexes
    # extract the ftp filename 
    # match to the lidar metrics filename (should be the same)
    # Create a vrt/sprc of all the matched filenames in the Metrics folder
            # Some of the HUCs might overlap with multiple indexes and we should 

### Previous code to create all indexes
# lidar_index_list <- list.files("Data/Lidar/Indexes", full.names = TRUE)
# lidar_index_all_sf <- lapply(lidar_index_list, \(x){st_read(x, quiet = TRUE) |>
#         st_transform("EPSG:6347") |>
#         dplyr::select(FILENAME, starts_with("COLLECT"), DIRECT_DL) |> 
#         st_make_valid()
#         }) |>
#     bind_rows()
# write_sf(lidar_index_all_sf, "Data/Lidar/NYS_Lidar_All_Indexes.gpkg", append = F)

lidar_index_all_sf_collect <- st_read("Data/Lidar/NYS_Lidar_All_Indexes.gpkg", quiet = T) |> 
    dplyr::mutate(COLLECTION = case_when(is.na(COLLECTION) ~ COLLECTION_NAME,
                                         .default = COLLECTION)) |> 
    dplyr::select(-COLLECTION_NAME) |> 
    st_buffer(0) |> 
    dplyr::filter(as.numeric(st_area(geom)) > 2240000)
lidar_index_all_sf_noe <- lidar_index_all_sf_collect[!st_is_empty(lidar_index_all_sf_collect), ]

lidar_huc <- function(huc_num){
    huc <- cluster_hucs[cluster_hucs$huc12 == huc_num, ]
    lidar_huc_fn <-  paste0(out_dir, "Lidar_cluster_", cluster_num, "_huc_", huc_num, ".tif")
    
    if(!file.exists(lidar_huc_fn)){
        message("New file created for: ", lidar_huc_fn)
        index_intersect <- st_intersects(lidar_index_all_sf_noe, huc, sparse = F)
        index_in_huc <- lidar_index_all_sf_noe[rowSums(index_intersect) > 0, ]
        lidar_fn <- tools::file_path_sans_ext(index_in_huc$FILENAME)
        message("length of total lidar filenames in index: ", length(lidar_fn))
        lidar_metrics_in_huc <- current_lidar_metrics[current_lidar_metrics_fn %in% lidar_fn]
        message("length of lidar filenames in huc ",huc_num, ": ", length(lidar_metrics_in_huc))
        # lidar_metrics_in_huc |>
        #     purrr::map(\(r) {
        #         target <- c("pct_below_0.5m", "pct_0.5_to_2m", "pct_2m_to_p95")
        #         available <- intersect(target, names(r))
        #         if (length(available) == 0) return(NULL)
        #         global(r[[available]], "max", na.rm = TRUE)
        #     })
        lidar_metrics_huc <- sprc(lidar_metrics_in_huc) |>
            terra::mosaic(fun = "mean") |>
            terra::crop(y = vect(huc), mask = TRUE)
        writeRaster(lidar_metrics_huc, lidar_huc_fn)
        #rm(lidar_metrics_huc)
        # return(lidar_metrics_huc)
    } else {
        message("Already file: ", lidar_huc_fn)
    }
    
}

lapply(huc_numbers, lidar_huc)

### testing
# int <- st_intersects(lidar_index_all_sf_noe, cluster_hucs[1,], sparse = F) |> rowSums() 
# l_int <- lidar_index_all_sf_noe[int > 0, ] 
# unique(l_int$COLLECTION)
# l_int_col <- (l_int[l_int$COLLECTION == "NYSGPO - New York Central Finer Lakes 2020", ])
# l_int_col2 <- (l_int[l_int$COLLECTION == "FEMA 2019", ])
# l_int_fn <- tools::file_path_sans_ext(l_int_col$FILENAME)
# l_cm <- current_lidar_metrics[current_lidar_metrics_fn %in% l_int_fn]
# plet(c(vect(l_int_col), vect(l_int_col2)))
# plet(c(vect(l_int_col), vect(cluster_hucs[1,])))
# plot(rast(l_cm[1]))
# plot(vrt(l_cm))

# ## Troubleshooting
# find_rasters_above_1_detail <- function(dir) {
# 
#     target_layers <- c("pct_2m_to_p95")
#     
#     files <- list.files(dir, full.names = TRUE)
#     
#     results <- files |>
#         purrr::map(\(f) {
#             r <- rast(f)
#             names(r) == names(t)
#             # r_sub <- r[[names(r) %in% target_layers]]
#             # maxvals <- global(r_sub, "max", na.rm = TRUE)
#             # flagged <- rownames(maxvals[maxvals$max > 1, , drop = FALSE])
#             # flagged
#          }) #|>
#         # purrr::set_names(basename(files)) |>
#         # purrr::keep(\(x) length(x) > 0)
#     
#     (results)
# }
# 
# find_rasters_above_1_detail("Data/Lidar/Metrics")
