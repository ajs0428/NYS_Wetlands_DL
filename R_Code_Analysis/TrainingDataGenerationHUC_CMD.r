#!/usr/bin/env Rscript

args = c(
        "Data/NWI/NY_Wetlands_6350_filterTrain.gpkg",
        "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
        "cluster"
        )

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

# test if there is at least one argument: if not, return an error
if (length(args)<4) {
    stop("At least three arguments must be supplied (input file).n", call.=FALSE)
 } # else if (length(args)==3) {
#     # default output file
#     args[1] = "Data/NWI/NY_Wetlands_6350.gpkg"
#     args[2] = "Data/NY_HUCS/NY_HUCS_12.gpkg"
# }

library(terra)
library(tidyverse)
library(tidyterra)


set.seed(11)


ny_nwi <- vect(args[1])
ny_hucs <- vect(args[2])

huc_nums <- values(ny_hucs[, "huc12"])[1:args[3],] # how do I tell a CMD line script how many and which HUC12 to run?


training_pts_func <- function(huc_num, nwi = ny_nwi, hucs = ny_hucs) {
    
    target_huc <- tidyterra::filter(hucs, huc12 == huc_num)
    #print(target_huc)
    
    nwi_huc_crop <- nwi |> terra::crop(y = target_huc) 
    #print(nwi_huc_crop)
    
    nwi_huc_filter <- nwi_huc_crop |> 
        dplyr::filter(WETLAND_TY == "Freshwater Forested/Shrub Wetland" | WETLAND_TY == "Freshwater Emergent Wetland")
    
    pts <- terra::spatSample(target_huc, method = "random", size = 1E3)
    
    nwi_pts_wet <- terra::mask(pts, nwi_huc_filter, inverse = FALSE) |> 
        terra::intersect(x = nwi_huc_filter |> terra::buffer(-1)) |> 
        dplyr::mutate(MOD_CLASS = case_when(WETLAND_TY == "Freshwater Forested/Shrub Wetland" ~ "FSW",
                                            .default = "EMW"),
                      COARSE_CLASS = "WET") |>
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    
    nwi_pts_upl <- terra::mask(pts, nwi_huc_crop |> buffer(5), inverse = TRUE) |> 
        dplyr::mutate(MOD_CLASS = "UPL",
                      COARSE_CLASS = "UPL") |>
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    
    nwi_pts_all <- rbind(nwi_pts_upl, nwi_pts_wet)
    #nwi_pts_all |> as.data.frame() |>  dplyr::group_by(MOD_CLASS) |> dplyr::summarise(count = n())
    
    numEMW <- length(nwi_huc_filter[nwi_huc_filter$WETLAND_TY == "Freshwater Emergent Wetland"])
    suppPoints <- nwi_huc_filter |>
        dplyr::filter(str_detect(WETLAND_TY, "Emergent")) |>
        terra::buffer(-10) |> # a negative buffer should remove points on the lines
        terra::as.points() |>
        sample(size = c(numEMW, 100)[which.max(c(numEMW, 100))])  |>
        dplyr::mutate(MOD_CLASS = "EMW",
                      COARSE_CLASS = "WET") |>
        dplyr::select(MOD_CLASS, COARSE_CLASS)

    nwi_pts_all_supp <- rbind(nwi_pts_all, suppPoints)
    
    #print(huc_num)
    #nwi_pts_all_supp |> as.data.frame() |>  dplyr::group_by(MOD_CLASS) |> dplyr::summarise(count = n())
    
    writeVector(target_huc, paste0("Data/NY_HUCS/", 
                                   str_replace_all(target_huc$name, pattern = "[ -]", "_"),
                                   "_", 
                                   huc_num,
                                   "_HUC.gpkg"), 
                overwrite = TRUE)
    writeVector(nwi_pts_all_supp, paste0("Data/Training_Data/", 
                                         str_replace_all(target_huc$name, pattern = "[ -]", "_"),
                                         "_", 
                                         huc_num,
                                         "_training_pts.gpkg"), overwrite = TRUE)
}

lapply(huc_nums, training_pts_func)
