### Image chips/patches for DL  

library(terra)
library(sf) 
library(dplyr)
library(tidyr)
library(stringr)
library(tidyterra)
library(readr)
library(future)
library(future.apply)


set.seed(11)

########################################################################################

args <- c(
    64,
    "Data/Training_Data/NHP_HUC_Wetlands_For_Field_Validation/", #Path to wetland polygons
    128,
    FALSE # This should be false and then re-run as TRUE once GIS edits have taken place
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) Cluster number for HUC groups:", args[1], "\n", 
    "2) path the reviewed training data :", args[2], "\n",
    "3) patch size :", args[3], "\n",
    "4) Create prediction raster patches?: ", args[4]
)


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files but maybe needed
########################################################################################
l_dem <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".tif", full.names = TRUE) 
l_dem_cluster <- l_dem[str_detect(l_dem, paste0("cluster_", args[1])) & !str_detect(l_dem, "wbt")]
l_dem_cluster_nums <- str_extract(l_dem, "(?<=cluster_)\\d+(?=_)") |> unique()

l_chm <- list.files("Data/CHMs/HUC_CHMs/", pattern = ".tif", full.names = TRUE) 
l_chm_cluster <- l_chm[str_detect(l_chm, paste0("cluster_", args[1]))]

l_naip <- list.files("Data/NAIP/HUC_NAIP_Processed/", pattern = ".tif", full.names = TRUE) 
l_naip_cluster <- l_naip[str_detect(l_naip, paste0("cluster_", args[1]))]

l_terr <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", 
                             pattern = paste0("cluster_", args[1], "_huc"), 
                             full.names = TRUE)
l_terr_cluster <- l_terr[str_detect(l_terr, "local") & !str_detect(l_terr, "10m|1000m")]
l_sat_cluster <- list.files("Data/Satellite/HUC_Processed_NY_Sentinel_Indices/", 
                            pattern = paste0("cluster_", args[1]),
                            full.names = TRUE)

l_wet <- list.files(args[2], pattern = ".gpkg$", full.names = TRUE) 
l_wet_cluster <- l_wet[str_detect(l_wet, paste0("cluster_", args[1]))]

print(l_wet_cluster)
length(l_naip_cluster) == length(l_dem_cluster)

logpath <- "Data/R_Patches_Vector/Vector_Patch_Checklist.csv"
########################################################################################
fct_df <- data.frame(ID = 0:4, MOD_CLASS = c("EMW", "FSW", "OWW", "SSW", "UPL"))

########################################################################################
set.seed(420)
filter_min_distance <- function(points, min_dist = 128) {
    n <- nrow(points)
    keep <- rep(TRUE, n)
    
    for (i in 1:(n - 1)) {
        if (keep[i]) {
            too_close <- st_is_within_distance(points[i, ], points[(i + 1):n, ], dist = min_dist)[[1]]
            if (length(too_close) > 0) {
                keep[(i + too_close)] <- FALSE
            }
        }
    }
    
    points[keep, ]
}
chip_patch_create <- function(wetland_file){
    ## Setup vars
    if(grepl("NWI", basename(wetland_file))){
        sourceWetlands <- "NWI"
    } else if(grepl("NHP", basename(wetland_file))){
        sourceWetlands <- "NHP"
    } else if(grepl("Laba", basename(wetland_file))){
        sourceWetlands <- "Laba"
    } else {
        sourceWetlands <- "Other"
    }
    patchsize = as.numeric(args[3])
    createRastPatches <- as.logical(args[4])
    huc_num <- str_extract(wetland_file, "(?<=huc_)\\d+")
    huc_poly <- sf::st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE,
                                  query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE huc12 = '", huc_num, "'"))
    
    target_wetlands <- st_read(wetland_file, quiet = TRUE) # target wetlands
    tw_centroid <- st_centroid(target_wetlands) |> st_geometry() |> st_cast(to = "MULTIPOINT") #centroid cast to multipoint
    
    tw_boundary <- st_boundary(target_wetlands) |> st_cast("LINESTRING") # cast to linestring
    # perimeter <- st_length(tw_boundary)
    # n_points <- pmax(0, as.integer(perimeter / 500)) # Divide the perimeter by 500m, round to nearest integer
    tw_b_line <- st_line_sample(tw_boundary, density = 1/500) # put n_points on the border
    
    tw_bl_point <- st_cast(tw_b_line, "POINT")
    tw_bl_point <- tw_bl_point[!st_is_empty(tw_bl_point)]
    tw_c_point <- st_cast(tw_centroid, "POINT")

    ### upland points and bounding boxes
    rand_pts <- st_sample(huc_poly, 10)
    target_wetlands_buffer <- st_buffer(target_wetlands, dist = 250)
    rand_pts_intersect <- st_intersects(rand_pts, target_wetlands_buffer, sparse = FALSE)
    pts_outside_target <- rowSums(rand_pts_intersect) == 0
    upl_pts <- rand_pts[pts_outside_target, ]
    upl_pts_box <- st_buffer(upl_pts, dist = patchsize, endCapStyle = "SQUARE") |> #set the size of the patch here (x2)
        st_sf()
    st_geometry(upl_pts_box) <- "geom"
    upl_pts_box["MOD_CLASS"] <- "UPL"
    upl_pts_box["huc12"] <- huc_num
    upl_pts_box["cluster"] <- as.integer(args[1])
    target_wetlands_uplands <- bind_rows(upl_pts_box, target_wetlands)
    
    ###combine points
    tw_bl_c_cmb <- rbind(
        st_sf(geometry = tw_bl_point),
        st_sf(geometry = tw_c_point),
        st_sf(geometry = upl_pts)
    )
    
    tw_bl_c_cmb_f <- filter_min_distance(tw_bl_c_cmb, patchsize*2) # filter out points that are too close
    tw_bl_c_cmbbuff <- st_buffer(tw_bl_c_cmb_f, dist = patchsize, endCapStyle = "SQUARE") 
    st_geometry(tw_bl_c_cmbbuff) <- "geom"   
    
    if(createRastPatches){
        dem_rast <- l_dem_cluster[grepl(huc_num, l_dem_cluster)] |> rast()
        set.names(dem_rast, "DEM")
        chm_rast <- l_chm_cluster[grepl(huc_num, l_chm_cluster)] |> rast()
        sat_rast <- l_sat_cluster[grepl(huc_num, l_sat_cluster)] |> rast()
        terr_rast <- l_terr_cluster[grepl(huc_num, l_terr_cluster)] |> rast()
        naip_rast <- l_naip_cluster[grepl(huc_num, l_naip_cluster)] |> rast()
        set.names(naip_rast, c("r", "g", "b", "nir", "n_ndvi", "n_ndwi"))
        
        tw_rast <- target_wetlands_uplands |> vect()  |>
            terra::rasterize(y = dem_rast, field = "MOD_CLASS", touches = TRUE)
        tw_rast_lc <- levels(tw_rast)[[1]][[2]] #character vector of levels present
        tw_rast_ln <- levels(tw_rast)[[1]][[1]] #numbers/integers of levels present
        fct_n <- fct_df[fct_df$MOD_CLASS %in% tw_rast_lc, ][,1] # subset the levels present from the full factor dataframe
        tw_rast_sub <- subst(tw_rast, from = tw_rast_ln, to = fct_n, raw = TRUE)
        #tw_rast_sub_int <- terra::as.int(tw_rast_sub)
        levels(tw_rast_sub) <- fct_df
        
        stack <- c(dem_rast, terr_rast, chm_rast, sat_rast, naip_rast, tw_rast_sub)
        # stack_fn <- paste0("Data/HUC_Raster_Stacks/HUC_DL_Stacks/", "cluster_", args[1], "_huc_", huc_num, "_stack.tif")
        # if(!file.exists(stack_fn)){
        #     writeRaster(stack, filename = stack_fn, overwrite = TRUE)
        # }
        
        for(i in seq_len(nrow(tw_bl_c_cmbbuff))){
            fn <- paste0("Data/R_Patches/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )
            fn_vector <- paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.gpkg" )
            fn_labels <- paste0("Data/R_Patches_Labels/", "labels_only_", sourceWetlands, "_cluster_", args[1], "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )
            
            # Regular Patches with all predictors
            if(!file.exists(fn)){
                crop(stack, vect(tw_bl_c_cmbbuff[i,]), mask = TRUE,
                     filename = fn,
                     overwrite = TRUE)
            } else {
                message("Already file ", fn)
            }
            #Labels only patches NO predictors
            if(!file.exists(fn_labels)){
                crop(tw_rast_sub, vect(tw_bl_c_cmbbuff[i,]), mask = TRUE,
                     filename = fn_labels,
                     overwrite = TRUE)
            } else {
                message("Already file ", fn_labels)
            }
        }
    } else {
        #### Vector polygon patches

        for(i in seq_len(nrow(tw_bl_c_cmbbuff))){
            fn_vector <- paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.gpkg" )
            if(!file.exists(fn_vector)){
                wet_patch <-  st_intersection(target_wetlands_uplands, tw_bl_c_cmbbuff[i,])
                upl_patch <- st_difference(tw_bl_c_cmbbuff[i,] |>
                                           mutate(MOD_CLASS = "UPL"),
                                       st_union(target_wetlands_uplands))
                st_geometry(upl_patch) <- "geom"
                wetupl_patch <- bind_rows(wet_patch, upl_patch) |>
                mutate(ReviewerName = "TBD",
                       Confidence = -999,
                       BoundariesAltered = NA,
                       Comments = "NoComment") |>
                dplyr::select(ReviewerName, Confidence, BoundariesAltered, Comments, MOD_CLASS)
                
                st_write(wetupl_patch, dsn = fn_vector, append = FALSE)
                
                if(file.exists(logpath)){
                    logfile <- read_csv(logpath, show_col_types = FALSE)
                    fn_to_add <- logfile |> filter(patch_file_name == basename(fn_vector))
                    if(nrow(fn_to_add) == 0){
                        fn_to_add_row <- data.frame(patch_file_name = basename(fn_vector),
                                                reviewer = "NAME",
                                                boundaries_altered = "TBD",
                                                confidence = "TBD")
                        # update_logfile <- bind_rows(fn_to_add_row, logfile)
                        write_csv(fn_to_add_row, logpath, append = TRUE)
                        } else {
                            message("Filename in log file")
                        }
                    }

            } else {
            message("Already file ", fn_vector)
                }
           }
    }
    
    fn_full_patch <- paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" )
    if(file.exists(fn_full_patch)){
        full_patch_file <- list.files("Data/R_Patches_Vector/", 
                                      full.names = TRUE, 
                                      pattern = paste0("_cluster_", args[1], "_huc_", huc_num, "_", "patch.*\\.gpkg$")) |> 
            purrr::map(st_read, quiet = TRUE) |> 
            bind_rows()
        st_write(full_patch_file, 
                 dsn =  paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" ),
                 append = TRUE)
    } else {
        full_patch_file <- list.files("Data/R_Patches_Vector/", 
                                      full.names = TRUE, 
                                      pattern = paste0("_cluster_", args[1], "_huc_", huc_num, "_", "patch.*\\.gpkg$")) |> 
            purrr::map(st_read, quiet = TRUE) |> 
            bind_rows()
        st_write(full_patch_file, 
                 dsn =  paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" ),
                 append = FALSE)
    }
        
    return(NULL)

}



### Parallel

if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
print(corenum)
options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr)

future_lapply(l_wet_cluster, chip_patch_create, 
              future.seed = TRUE,
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
              future.globals = TRUE)

### Non-parallel
# system.time({lapply(l_wet_cluster, chip_patch_create)})


# l_patches <- list.files("Data/R_Patches_Vector")
# 
# check_df <- data.frame(patch_file_name = l_patches,
#                        reviewer = rep("NAME", length(l_patches)),
#                        boundaries_altered = rep("TBD", length(l_patches)),
#                        confidence = rep("TBD", length(l_patches)))
# 
# readr::write_csv(check_df, "Data/R_Patches_Vector/Vector_Patch_Checklist.csv")
# ### Checks
# list_patches <- list.files("Data/R_Patches_Labels/", full.names = T)
# lapply(list_patches, \(x) rast(x))
# lp <- lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist()
# # lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist() |> table()
# 
# le <- lapply(list_patches, FUN = \(x) {rast(x, lyrs = "MOD_CLASS") |> values() |> unique() |> nrow()}) |> unlist()
# 
# list_patches[le == 1]
# list_patches[lp < 27]
