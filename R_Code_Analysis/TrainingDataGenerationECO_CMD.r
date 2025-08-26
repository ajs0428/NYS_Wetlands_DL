#!/usr/bin/env Rscript

args <- c(
    "Data/NWI/NY_Wetlands_6350_filterTrain.gpkg",
    "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
    "cluster"
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) path to full NWI dataset:", args[1], "\n", 
    "2) path to areas of interest (Ecoregions or Other):", args[2], "\n",
    "3) The data field name of the areas to subset:", args[3], "\n")

# test if there is at least one argument: if not, return an error
if (length(args)<3) {
    stop("At least three arguments must be supplied (input file).n", call.=FALSE)
 }

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)

corenum <-  16
stopifnot("Too many cores specified" = corenum <= future::availableCores()[[1]])
options(future.globals.maxSize= 1.0 * 1e9)
plan(multisession, workers = corenum) # number of cores is decided by availableCores() and should work with Slurm scheduler
# this should probably be an argument for bash


set.seed(11)

# filter the NWI down to the targeted wetlands
ny_nwi <- st_read(args[1], quiet = TRUE) 
if(st_crs(ny_nwi) != st_crs("EPSG:6347")){
    ny_nwi <- st_transform(ny_nwi, "EPSG:6347")
} else {
    print("No reprojection to EPSG:6347")
}
    
# The areas of interest are projected to match the NWI
ny_areas <- st_read(args[2]) |> st_transform("EPSG:6347")

# The list of area names/IDs
ID <- args[3]
area_ids <- ny_areas[, ID][[1]] |> na.omit() |> unique()

# The function should take a arguments to subset ecoregions/study areas and produce NWI sample points within them
    #### Separate surface water to be used to filter out unwanted poins 
    #### Generate training data from wetlands but filter upland points by full NWI
training_pts_func <- function(ids, areas = ny_areas, nwi = ny_nwi) {
    
    # The target area is the single ecoregion or area from within NY State
    target <- filter(areas, !!as.symbol(ID) == ids[1])
    #print(ID)
    
    # target_name is for saving the files
    target_name <- ids[[1]]
    # print(target_name)
    #only select surface water to remove from uplands
    nwi_upl_mask <- nwi |> 
        filter(str_detect(ATTRIBUTE, "L1|L2|R1|R2|R3|R4|R5|E1|E2")) |> 
        filter(str_detect(WETLAND_TY, "Pond|Marine|Lake|Other")) |> 
        
        st_filter(target)
    # Reclassify wetlands in the cropped NWI to Forested, Emergent, or Scrub Shrub
    nwi_area_filter <- nwi |> 
        filter(!str_detect(ATTRIBUTE, "L1|L2|R1|R2|R3|R4|R5|E1|E2")) |> 
        filter(!str_detect(WETLAND_TY, "Pond|Marine|Lake|Other")) |> 
        st_filter(target) |>
      mutate(WetClass = case_when(str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "ScrubShrub",
                                  str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "Emergent",
                                  str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "Forested",
                                  str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "ForestedScrub",
                                  str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "EmergentScrub",
                                  .default = ATTRIBUTE))

    # The number of different wetland polygons in the cropped NWI
    numEMW <- nrow(nwi_area_filter[nwi_area_filter$WetClass == "Emergent",])
    numFSW <- nrow(nwi_area_filter[nwi_area_filter$WetClass == "Forested",])
    numSSW <- nrow(nwi_area_filter[nwi_area_filter$WetClass == "ScrubShrub",])
    print(cat("emergent",numEMW))
    print(cat("forest",numFSW))
    print(cat("shrub",numSSW))
    # The NWI wetland points are created turning NWI polygons to points
    nwi_pts_wet <- nwi_area_filter |>
        dplyr::mutate(geom = case_when(Shape_Area > 3000 ~ st_buffer(geom, -10), # a negative buffer should remove points on the lines
                                       .default = geom)) %>%
        filter(!st_is_empty(.)) |> 
        st_sample(size = sum(numEMW, numFSW, numSSW, na.rm = TRUE)*10) |> # 10 samples per number of wetlands? But also is random so will account for size
        st_sf() |>
        st_set_geometry("geom") |> 
        st_join(nwi_area_filter[,"WetClass"]) |> 
        dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                            WetClass == "Forested" ~ "FSW",
                                            WetClass == "ScrubShrub" ~ "SSW",
                                            .default = "Other"),
                      COARSE_CLASS = "WET") |> # COARSE CLASS is for simple modeling
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    # print(nwi_pts_wet)
    
    # Upland points are defined as outside the NWI polygons
        # Might have some commission error/included wetlands, so there are many of these
        # Setting to 5x the number of wetland points 
    pts <- st_sample(st_bbox(target), size = sum(numEMW, numFSW, numSSW, na.rm = TRUE)*10*5)
      # reverse mask the random number of points outside NWI polygons
    nwi_pts_upl <- pts[lengths(st_intersects(pts, nwi_upl_mask |> st_buffer(10), sparse = TRUE)) ==0,] |> 
        st_sf() |> 
        st_set_geometry("geom") |> 
        dplyr::mutate(MOD_CLASS = "UPL",
                      COARSE_CLASS = "UPL") |>
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    # print(nwi_pts_upl)
    # Combine upland and wetland points
    nwi_pts_all <- rbind(nwi_pts_upl, nwi_pts_wet)

    # The number of wetland points to balance the classes a bit
    numFSW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "FSW", ])
    numEMW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "EMW", ])
    numSSW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "SSW", ])
    print(cat("numFSW_pts:", numFSW_pts))
    print(cat("numEMW_pts:", numEMW_pts))
    print(cat("numSSW_pts:", numSSW_pts))
    # If there are fewer than half of emergent and scrub/shrub vs. forested then supplement the points by sampling
        # additional emergent polygons
    if(numEMW_pts < 0.5*numFSW_pts & numSSW_pts < 0.5*numFSW_pts ){
        suppPointsEMW <- nwi_area_filter |>
            dplyr::filter(str_detect(WetClass, "Emergent")) |>
            dplyr::filter(Shape_Area < 5000) |> #try to sample smaller wetlands that might have been missed
            st_buffer(-1) %>% # a smaller negative buffer should remove points on the lines
            filter(!st_is_empty(.)) |> 
            st_sample(size = ceiling(numEMW_pts/2)) |> # increase by 33%?
            st_sf() |> 
            st_set_geometry("geom") |> 
            dplyr::mutate(MOD_CLASS = "EMW",
                          COARSE_CLASS = "WET") |>
            dplyr::select(MOD_CLASS, COARSE_CLASS)
        suppPointsSSW <- nwi_area_filter |>
            dplyr::filter(str_detect(WetClass, "ScrubShrub")) |>
            dplyr::filter(Shape_Area < 5000) |>  #try to sample smaller wetlands that might have been missed
            st_buffer(-1) %>% # a smaller negative buffer should remove points on the lines
            filter(!st_is_empty(.)) |> 
            st_sample(size = ceiling(numSSW_pts/2)) |> # increase by 33%?
            st_sf() |> 
            st_set_geometry("geom") |> 
            dplyr::mutate(MOD_CLASS = "SSW",
                          COARSE_CLASS = "WET") |>
            dplyr::select(MOD_CLASS, COARSE_CLASS)

        nwi_pts_all_supp <- rbind(nwi_pts_all, suppPointsEMW, suppPointsSSW)
    } else { #don't change if > half of forested/scrub/shrub
        nwi_pts_all_supp <- nwi_pts_all
    }
    
    # Summary of point distribution
    print(nwi_pts_all_supp |> as.data.frame() |>  dplyr::group_by(MOD_CLASS) |> dplyr::summarise(count = n()))
     
    st_write(obj = nwi_pts_all_supp, dsn = paste0("Data/Training_Data/",
                                         args[3],"_",
                                         target_name,
                                         #"_",
                                         #ids,
                                         "_training_pts.gpkg"), append = FALSE)
}

system.time({future_lapply(area_ids, training_pts_func, future.seed=TRUE)})

# lapply(area_ids[1:2], training_pts_func)
