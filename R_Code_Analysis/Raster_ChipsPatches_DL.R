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
    "Data/R_Patches_Vector_Reviewed/", #Path to GIS reviewed wetland vector patches
    128
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "2) path the reviewed training data :", args[1], "\n",
    "3) patch size :", args[2], "\n"
)


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files but maybe needed
########################################################################################
l_wet <- list.files(args[1], pattern = ".gpkg$", full.names = TRUE) 
l_wet_cluster_nums <- sub(".*cluster_(\\d+).*", "\\1", l_wet)
l_wet_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_wet)
l_wet_cluster <- l_wet[l_wet_extracted_clusters %in% as.character(l_wet_cluster_nums)]
print(l_wet_cluster)

l_dem <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".tif", full.names = TRUE) 
l_dem_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_dem)
l_dem_cluster <- l_dem[l_dem_extracted_clusters %in% as.character(l_wet_cluster_nums) & !str_detect(l_dem, "wbt")]
l_dem_cluster_nums <- str_extract(l_dem, "(?<=cluster_)\\d+(?=_)") |> unique() # All the DEM clusters

l_chm <- list.files("Data/CHMs/HUC_CHMs/", pattern = ".tif", full.names = TRUE) 
l_chm_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_chm)
l_chm_cluster <- l_chm[l_chm_extracted_clusters %in% as.character(l_wet_cluster_nums)]

l_naip <- list.files("Data/NAIP/HUC_NAIP_Processed/", pattern = ".tif", full.names = TRUE) 
l_naip_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_naip)
l_naip_cluster <- l_naip[l_naip_extracted_clusters %in% as.character(l_wet_cluster_nums)]

l_terr <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", 
                             # pattern = paste0("cluster_", args[1], "_huc"), 
                             full.names = TRUE)
l_terr_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_terr)
l_terr_cluster <- l_terr[l_terr_extracted_clusters %in% as.character(l_wet_cluster_nums) & 
                             str_detect(l_terr, "local") & 
                             !str_detect(l_terr, "10m|1000m")]

l_sat <- list.files("Data/Satellite/HUC_Processed_NY_Sentinel_Indices/", 
                            # pattern = paste0("cluster_", args[1]),
                            full.names = TRUE)
l_sat_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_sat)
l_sat_cluster <- l_sat[l_sat_extracted_clusters %in% as.character(l_wet_cluster_nums)]


length(l_naip_cluster) == length(l_dem_cluster) & length(l_dem_cluster) == length(l_sat_cluster)

logpath <- "Data/R_Patches_Vector/Vector_Patch_Checklist.csv"
########################################################################################
fct_df <- data.frame(ID = 0:4, MOD_CLASS = c("EMW", "FSW", "OWW", "SSW", "UPL"))
patchsize = as.numeric(args[2])
########################################################################################
set.seed(420)

rast_chip_patch_create <- function(wetland_file){
    ## Setup vars
    if (grepl("NWI", basename(wetland_file))) {
        sourceWetlands <- "NWI"
    } else if (grepl("NHP", basename(wetland_file))) {
        sourceWetlands <- "NHP"
    } else if (grepl("Laba", basename(wetland_file))) {
        sourceWetlands <- "Laba"
    } else {
        sourceWetlands <- sub("_.*", "", tools::file_path_sans_ext(basename(wetland_file)))
    }
    patchsize = as.numeric(args[2])
    huc_num <- str_extract(wetland_file, "(?<=huc_)\\d+")
    cluster_num <- str_extract(wetland_file, "(?<=cluster_)\\d+")
    # huc_poly <- sf::st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE,
    #                               query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE huc12 = '", huc_num, "'"))

    dem_rast <- l_dem_cluster[grepl(huc_num, l_dem_cluster) & grepl(paste0("cluster_", cluster_num), l_dem_cluster)] |> rast()
    set.names(dem_rast, "DEM")
    chm_rast <- l_chm_cluster[grepl(huc_num, l_chm_cluster) & grepl(paste0("cluster_", cluster_num), l_chm_cluster)] |> rast()
    sat_rast <- l_sat_cluster[grepl(huc_num, l_sat_cluster)& grepl(paste0("cluster_", cluster_num), l_sat_cluster)] |> rast()
    terr_rast <- l_terr_cluster[grepl(huc_num, l_terr_cluster)& grepl(paste0("cluster_", cluster_num), l_terr_cluster)] |> rast()
    naip_rast <- l_naip_cluster[grepl(huc_num, l_naip_cluster)& grepl(paste0("cluster_", cluster_num), l_naip_cluster)] |> rast()
    set.names(naip_rast, c("r", "g", "b", "nir", "n_ndvi", "n_ndwi"))
    
    stack <- c(dem_rast, terr_rast, chm_rast, sat_rast, naip_rast)
    stack_fn <- paste0("Data/HUC_Raster_Stacks/HUC_DL_Stacks/", "cluster_", cluster_num, "_huc_", huc_num, "_stack.tif")
    if (!file.exists(stack_fn)) {
        writeRaster(stack, filename = stack_fn, overwrite = TRUE)
    }
    
    ### Union all the polygons then rejoin and separate as groups 
        ### so that each patch of touching polygons is a separate 
            ### object that can be used to crop the rasters
    tw <- st_read(l_wet_cluster[grepl(huc_num, l_wet_cluster) & grepl(paste0("cluster_", cluster_num), l_wet_cluster)], quiet = TRUE)
    tw_valid <- tw[st_is_valid(tw), ]
    tw_union <- tw_valid |>
        st_union() |>
        st_cast("POLYGON") |>
        st_as_sf() |>
        mutate(group_id = row_number())
    st_geometry(tw_union) <- "geom"
    tw_union_area <- tw_union |> 
        mutate(area = as.numeric(st_area(geom))) |> 
        filter(area >= ((patchsize*2)**2)-0.5) #remove patches that are smaller than the 256*256 dimensions
    tw_grouped_list <- tw_valid |> st_join(tw_union_area, left = FALSE) |>
        filter(st_is_valid(geom)) |>
        group_split(group_id)
    
    #### Each patch should be a separate file that is patchsize*2 x patchsize*2
    for(i in seq_along(tw_grouped_list)){
        skip_to_next <- FALSE
        tw_vect <- vect(tw_grouped_list[[i]]) 
        
        tryCatch({

            dem_crop <- crop(dem_rast, tw_vect, touches = TRUE, mask = TRUE)

            tw_rast <- tw_vect  |>
                terra::rasterize(y = dem_crop, field = "MOD_CLASS", touches = TRUE)
            tw_rast_lc <- levels(tw_rast)[[1]][[2]] #character vector of levels present
            tw_rast_ln <- levels(tw_rast)[[1]][[1]] #numbers/integers of levels present
            fct_n <- fct_df[fct_df$MOD_CLASS %in% tw_rast_lc, ][,1] # subset the levels present from the full factor dataframe
            tw_rast_sub <- subst(tw_rast, from = tw_rast_ln, to = fct_n, raw = TRUE)
            levels(tw_rast_sub) <- fct_df

            fn <- paste0("Data/R_Patches/", sourceWetlands,"_cluster_", cluster_num, "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )
            fn_labels <- paste0("Data/R_Patches_Labels/", "labels_only_", sourceWetlands, "_cluster_", cluster_num, "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )

            # Regular Patches with all predictors
            if(!file.exists(fn)){
                tryCatch({
                    cropped_stack <- crop(stack, tw_vect, mask = TRUE)
                    cropped_stack_labeled <- c(cropped_stack, tw_rast_sub)
                    writeRaster(cropped_stack_labeled, filename = fn, overwrite = TRUE)
                }, error = function(e) { message("Cropping Stack")
                                                 skip_to_next <<- TRUE}
                )
                if(skip_to_next) { next }
            } else {
                message("Already file ", fn)
            }

            #Labels only patches NO predictors
            if(!file.exists(fn_labels)){
                writeRaster(tw_rast_sub, filename = fn_labels, overwrite = TRUE)
                } else {
                    message("Already file ", fn_labels)
                    }
            },
        error = function(e) {
            message("Error: ", conditionMessage(e))
            return(NA)
        })
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

future_lapply(l_wet_cluster, rast_chip_patch_create,
              future.seed = TRUE,
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
              future.globals = TRUE)

### Non-parallel
# system.time({lapply(l_wet_cluster, rast_chip_patch_create)})


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



