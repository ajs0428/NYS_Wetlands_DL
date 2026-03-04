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
library(purrr)

set.seed(11)

########################################################################################

args <- c(
    225,
    "Data/Training_Data/ADK_HUC_Processed/", #Path to wetland polygons
    128 # Patch size radius
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) Cluster number for HUC groups:", args[1], "\n", 
    "2) path the reviewed training data :", args[2], "\n",
    "3) patch size :", args[3], "\n"
)


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files but maybe needed
########################################################################################
l_wet <- list.files(args[2], pattern = ".gpkg$", full.names = TRUE) |> keep(\(x) str_detect(x, "ADK_WCT"))
l_wet_cluster <- l_wet[str_detect(l_wet, paste0("cluster_", args[1]))]

print(l_wet_cluster)

logpath <- "Data/R_Patches_Vector/Vector_Patch_Checklist.csv"
########################################################################################
# fct_df <- data.frame(ID = 0:4, MOD_CLASS = c("EMW", "FSW", "OWW", "SSW", "UPL"))
fct_df <- data.frame(ID = 0:3, MOD_CLASS = c("EMW", "FSW", "SSW", "UPL"))
patchsize = as.numeric(args[3])
########################################################################################
set.seed(420)

vect_chip_patch_create <- function(wetland_file){
    ## Setup vars
    if(grepl("NWI", basename(wetland_file))){
        sourceWetlands <- "NWI"
    } else if(grepl("NHP", basename(wetland_file))){
        sourceWetlands <- "NHP"
    } else if(grepl("Laba", basename(wetland_file))){
        sourceWetlands <- "Laba"
    } else if(grepl("ADK_WCT", basename(wetland_file))){
        sourceWetlands <- "ADK_WCT"
    } else if(grepl("ADK_regulated", basename(wetland_file))){
        sourceWetlands <- "ADK_regulated"
    } else {
        sourceWetlands <- sub("_.*", "", tools::file_path_sans_ext(basename(wetland_file)))
    }
    message(sourceWetlands)
    huc_num <- str_extract(wetland_file, "(?<=huc_)\\d+")
    huc_poly <- sf::st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE,
                                  query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE huc12 = '", huc_num, "'"))
    
    target_wetlands <- st_read(wetland_file, quiet = TRUE) |> # target wetlands
        filter(MOD_CLASS != "OWW")
    
    tw_centroid <- st_centroid(target_wetlands) |> st_geometry() |> st_cast(to = "MULTIPOINT") #centroid cast to multipoint
    
    tw_boundary <- st_boundary(target_wetlands) |> st_cast("LINESTRING") # cast to linestring
    tw_b_line <- st_line_sample(tw_boundary, density = 1/512) # put n_points on the border
    
    tw_bl_point <- st_cast(tw_b_line, "POINT")
    tw_bl_point <- tw_bl_point[!st_is_empty(tw_bl_point)]
    tw_bl_point <- tw_bl_point[rowSums(st_is_within_distance(tw_bl_point,
                                                           dist = 128,
                                                           sparse = FALSE,
                                                           remove_self = T)) == 0,] # filter out points that are too close
    tw_c_point <- st_cast(tw_centroid, "POINT")
    
    tw_c_point <- tw_c_point[rowSums(st_is_within_distance(tw_c_point,
                                                               dist = 128,
                                                               sparse = FALSE,
                                                               remove_self = T)) == 0,] # filter out points that are too close

    # ### upland points and bounding boxes
    # rand_pts <- st_sample(huc_poly, 10)
    # target_wetlands_buffer <- st_buffer(target_wetlands, dist = 250)
    # rand_pts_intersect <- st_intersects(rand_pts, target_wetlands_buffer, sparse = FALSE)
    # pts_outside_target <- rowSums(rand_pts_intersect) == 0
    # upl_pts <- rand_pts[pts_outside_target, ]
    # upl_pts_box <- st_buffer(upl_pts, dist = patchsize, endCapStyle = "SQUARE") |> #set the size of the patch here (x2)
    #     st_sf()
    # st_geometry(upl_pts_box) <- "geom"
    # upl_pts_box["MOD_CLASS"] <- "UPL"
    # upl_pts_box["huc12"] <- huc_num
    # upl_pts_box["cluster"] <- as.integer(args[1])
    # target_wetlands_uplands <- bind_rows(upl_pts_box, target_wetlands)
    target_wetlands_uplands <- target_wetlands
    ###combine points
    print(length(tw_bl_point))
    print(length(tw_c_point))
    # print(length(upl_pts))
    tw_bl_c_cmb <- rbind(
        st_sf(geometry = tw_bl_point),
        st_sf(geometry = tw_c_point) # ,
        # st_sf(geometry = upl_pts)
    )
    
    tw_bl_c_cmbbuff <- st_buffer(tw_bl_c_cmb, dist = patchsize, endCapStyle = "SQUARE")
    st_geometry(tw_bl_c_cmbbuff) <- "geom"
    tw_bl_c_cmbbuff_o <- tw_bl_c_cmbbuff[rowSums(st_overlaps(tw_bl_c_cmbbuff, sparse = F)) == 0, ]
    
    #### Vector polygon patches

    for(i in seq_len(nrow(tw_bl_c_cmbbuff_o))){
        fn_vector <- paste0("Data/R_Patches_Vector/individual_patches/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.gpkg" )
        if(!file.exists(fn_vector)){
            wet_patch <-  st_intersection(target_wetlands_uplands, tw_bl_c_cmbbuff_o[i,])
            upl_patch <- st_difference(tw_bl_c_cmbbuff_o[i,] |>
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

            # if(!file.exists(logpath)){
            #     logfile <- read_csv(logpath, show_col_types = FALSE)
            #     fn_to_add <- logfile |> filter(patch_file_name == basename(fn_vector))
            #     if(nrow(fn_to_add) == 0){
            #         fn_to_add_row <- data.frame(patch_file_name = basename(fn_vector),
            #                                 reviewer = "NAME",
            #                                 boundaries_altered = "TBD",
            #                                 confidence = "TBD")
            #         # update_logfile <- bind_rows(fn_to_add_row, logfile)
            #         write_csv(fn_to_add_row, logpath, append = TRUE)
            #         } else {
            #             message("Filename in log file")
            #         }
            #     }

        } else {
        message("Already file ", fn_vector)
            }
       }

    fn_full_patch <- paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" )
    if(!file.exists(fn_full_patch)){
        full_patch_file <- list.files("Data/R_Patches_Vector/individual_patches/",
                                      full.names = TRUE,
                                      pattern = paste0("_cluster_", args[1], "_huc_", huc_num, "_", "patch.*\\.gpkg$")) |>
            purrr::map(st_read, quiet = TRUE) |>
            bind_rows()
        st_write(full_patch_file,
                 dsn =  paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" ),
                 append = FALSE)
    } else {
        # full_patch_file <- list.files("Data/R_Patches_Vector/",
        #                               full.names = TRUE,
        #                               pattern = paste0("_cluster_", args[1], "_huc_", huc_num, "_", "patch.*\\.gpkg$")) |>
        #     purrr::map(st_read, quiet = TRUE) |>
        #     bind_rows()
        # st_write(full_patch_file,
        #          dsn =  paste0("Data/R_Patches_Vector/", sourceWetlands,"_cluster_", args[1], "_huc_", huc_num, "_", patchsize*2, "m.gpkg" ),
        #          append = FALSE)
        message("Already file")
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

future_lapply(l_wet_cluster, vect_chip_patch_create,
              future.seed = TRUE,
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
              future.globals = TRUE)

### Non-parallel
# system.time({lapply(l_wet_cluster[[1]], vect_chip_patch_create)})


#### Checks
# l_patches <- list.files("Data/R_Patches_Vector")
# 
# check_df <- data.frame(patch_file_name = l_patches,
#                        reviewer = rep("NAME", length(l_patches)),
#                        boundaries_altered = rep("TBD", length(l_patches)),
#                        confidence = rep("TBD", length(l_patches)))
# 
# readr::write_csv(check_df, "Data/R_Patches_Vector/Vector_Patch_Checklist.csv")
# 
# list_patches <- list.files("Data/R_Patches_Labels/", full.names = T)
# lapply(list_patches, \(x) rast(x))
# lp <- lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist()
# # lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist() |> table()
# 
# le <- lapply(list_patches, FUN = \(x) {rast(x, lyrs = "MOD_CLASS") |> values() |> unique() |> nrow()}) |> unlist()
# 
# list_patches[le == 1]
# list_patches[lp < 27]
