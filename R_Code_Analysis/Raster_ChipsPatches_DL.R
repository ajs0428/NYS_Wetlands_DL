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

source("R_Code_Analysis/huc_stack.R") # shared in-memory stack recipe

set.seed(11)

########################################################################################

args <- c(
    "Data/Training_Data/R_Patches_Vector_Reviewed/", #Path to GIS reviewed wetland vector patches
    128, # patch size 1/2
    250 # cluster subset options include number or NULL for any
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

patchPath <- args[1]
patchSize <- args[2]
clusterSubset <- args[3]

message("these are the arguments: \n", 
    "1) path the reviewed training data :", patchPath, "\n",
    "2) patch size :", patchSize, "\n",
    "3) cluster number :", clusterSubset, "\n"
)


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files but maybe needed
########################################################################################
l_wet <- list.files(patchPath, pattern = ".gpkg$", full.names = TRUE) 
l_wet_cluster_nums <- sub(".*cluster_(\\d+).*", "\\1", l_wet) |> unique()
l_wet_extracted_clusters <- sub(".*cluster_(\\d+)_.*", "\\1", l_wet)
l_wet_cluster <- l_wet[grepl(paste0("cluster_", clusterSubset, "_"), l_wet)]
print(l_wet_cluster)

########################################################################################
# fct_df <- data.frame(ID = 0:4, MOD_CLASS = c("EMW", "FSW", "OWW", "SSW", "UPL"))
fct_df <- data.frame(ID = 0:3, MOD_CLASS = c("EMW", "FSW", "SSW", "UPL"))
patchsize = as.numeric(patchSize)
########################################################################################
set.seed(420)

rast_chip_patch_create <- function(wetland_file){
    setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
    source("R_Code_Analysis/huc_stack.R") # ensure recipe is available in callr workers
    # Cap terra memory well below the per-task cgroup (mem-per-cpu * cpus-per-task,
    # shared across callr workers). terra otherwise sizes off total node RAM, not
    # the SLURM allocation, and the in-memory resample gets OOM-killed.
    terraOptions(memmax = 20, memfrac = 0.4, tempdir = "Data/tmp")
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
    patchsize = as.numeric(patchSize)
    huc_num <- str_extract(wetland_file, "(?<=huc_)\\d+")
    cluster_num <- str_extract(wetland_file, "(?<=cluster_)\\d+")
    message("HUC num: ", huc_num)
    if(cluster_num != clusterSubset & !is.null(clusterSubset)){
        message("skip this cluster and huc, selecting cluster: ", clusterSubset)
        return(invisible(NULL))
    } else if(cluster_num != clusterSubset & is.null(clusterSubset)) {
        message("Processing for all clusters in folder")
    }
    
    ## Resolve the per-HUC source rasters (lazy pointers). The stack is built
    ## in memory per patch below via build_huc_stack_patch() -- no *_stack.tif
    ## is read or written, so source rasters are never duplicated on disk.
    paths <- huc_source_paths(huc_num, cluster_num)
    if (!huc_sources_ready(paths, huc_num)) {
        message("Skipping HUC ", huc_num, ": one or more source datasets missing")
        return(invisible(NULL))
    }

    ### Union all the polygons then rejoin and separate as groups
        ### so that each patch of touching polygons is a separate
            ### object that can be used to crop the rasters
    tw <- st_read(l_wet_cluster[grepl(huc_num, l_wet_cluster) & grepl(sourceWetlands, l_wet_cluster)], quiet = TRUE)
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
    tw_grouped_list <- tw_valid |> st_join(tw_union_area, left = FALSE) %>%
        dplyr::filter(st_is_valid(.)) |>
        dplyr::group_split(group_id)
    
    #### Each patch should be a separate file that is patchsize*2 x patchsize*2
    for(i in seq_along(tw_grouped_list)){
        # message("The number is ", i)
        skip_to_next <- FALSE
        tw_vect <- vect(tw_grouped_list[[i]])

        tryCatch({

            # Build the predictor stack for just this patch window, in memory.
            stack <- build_huc_stack_patch(paths, tw_vect)
            dem_crop <- stack[["DEM"]]

            tw_rast <- tw_vect  |>
                terra::rasterize(y = dem_crop, field = "MOD_CLASS", touches = TRUE)
            tw_rast_lc <- levels(tw_rast)[[1]][[2]] #character vector of levels present
            tw_rast_ln <- levels(tw_rast)[[1]][[1]] #numbers/integers of levels present
            fct_n <- fct_df[fct_df$MOD_CLASS %in% tw_rast_lc, ][,1] # subset the levels present from the full factor dataframe
            tw_rast_sub <- subst(tw_rast, from = tw_rast_ln, to = fct_n, raw = TRUE)
            levels(tw_rast_sub) <- fct_df

            fn <- paste0("Data/Training_Data/R_Patches/", sourceWetlands,"_cluster_", cluster_num, "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )
            
            # fn_labels <- paste0("Data/Training_Data/R_Patches_Labels/", "labels_only_", sourceWetlands, "_cluster_", cluster_num, "_huc_", huc_num, "_patch_", i, "_", patchsize*2, "m.tif" )

            # Regular Patches with all predictors
            #if(!file.exists(fn)){
                tryCatch({
                    cropped_stack_labeled <- c(stack, tw_rast_sub)
                    writeRaster(cropped_stack_labeled, filename = fn, overwrite = TRUE)
                }, error = function(e) { message("Cropping Stack")
                                                 skip_to_next <<- TRUE}
                )
                if(skip_to_next) { next }
            # } else {
            #     message("Already file ", fn)
            # }

            # #Labels only patches NO predictors
            # if(!file.exists(fn_labels)){
            #     writeRaster(tw_rast_sub, filename = fn_labels, overwrite = TRUE)
            #     } else {
            #         message("Already file ", fn_labels)
            #         }
            },
        error = function(e) {
            message("Error: ", conditionMessage(e))
            return(NA)
        })
    }

    return(NULL)

}

### Non-parallel
# system.time({lapply(l_wet_cluster, rast_chip_patch_create)})
# 
# l_dem_cluster[[1]] |> rast() |> plot()
# l_hydro_cluster[[1]] |> rast() |> plot()
# l_chm_cluster[[1]] |> rast() |> plot()
# l_naip_cluster[[1]] |> rast() |> plot()
# l_sat_cluster[[1]] |> rast() |> plot()


### Parallel

slurm_cpus <- Sys.getenv("SLURM_CPUS_PER_TASK", unset = "")

if (nzchar(slurm_cpus)) {
  corenum <- as.integer(slurm_cpus)
} else {
  corenum <- min(future::availableCores(), 4)
}

print(corenum)
options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

future_lapply(l_wet_cluster, rast_chip_patch_create,
              future.seed = TRUE,
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr", "purrr"),
              future.globals = TRUE
              # future.globals = list(
              #   l_chm_cluster = l_chm_cluster,
              #   l_dem_cluster = l_dem_cluster,
              #   l_lidar_cluster = l_lidar_cluster,
              #   l_sat_cluster = l_sat_cluster,
              #   l_terr_cluster = l_terr_cluster,
              #   l_hydro_cluster = l_hydro_cluster,
              #   args = args,
              #   fct_df = fct_df
              # )
              )

### Checks 
# l_patches <- list.files("Data/Training_Data/R_Patches_Vector")
# 
# check_df <- data.frame(patch_file_name = l_patches,
#                        reviewer = rep("NAME", length(l_patches)),
#                        boundaries_altered = rep("TBD", length(l_patches)),
#                        confidence = rep("TBD", length(l_patches)))
# 
# readr::write_csv(check_df, "Data/Training_Data/R_Patches_Vector/Vector_Patch_Checklist.csv")
# ### Checks
# list_patches <- list.files("Data/Training_Data/R_Patches_Labels/", full.names = T)
# lapply(list_patches, \(x) rast(x))
# lp <- lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist()
# # lapply(list_patches, FUN = \(x) {rast(x) |> nlyr()}) |> unlist() |> table()
# 
# le <- lapply(list_patches, FUN = \(x) {rast(x, lyrs = "MOD_CLASS") |> values() |> unique() |> nrow()}) |> unlist()
# 
# list_patches[le == 1]
# list_patches[lp < 27]



