#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here
cat("these are the arguments: \n", 
    "1) path to full NWI dataset:", args[1], "\n", 
    "2) path to areas of interest (Ecoregions or Other)", args[2], "\n",
    "3) The data field name of the areas to subset", args[3], "\n")

# test if there is at least one argument: if not, return an error
if (length(args)<3) {
    stop("At least three arguments must be supplied (input file).n", call.=FALSE)
 }

library(terra)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)

corenum <-  16
stopifnot("Too many cores specified" = corenum <= future::availableCores()[[1]])
plan(multicore, workers = corenum) # number of cores is decided by availableCores() and should work with Slurm scheduler
# this should probably be an argument for bash


set.seed(11)

# filter the NWI down to the targeted wetlands
ny_nwi <- vect(args[1]) |> 
  filter(!str_detect(ATTRIBUTE, "L1|L2|R1|R2|R3|R4|R5|E1|E2")) |> 
  filter(!str_detect(WETLAND_TY, "Pond|Marine|Lake|Other"))

# The areas of interest are projected to match the NWI
ny_areas <- vect(args[2]) |> terra::project(crs(ny_nwi))

# The list of area names/IDs
ID <- args[3]
area_ids <- values(ny_areas[, ID]) |> na.omit()
area_ids <- area_ids[[1]]

# The function should take a arguments to subset ecoregions/study areas and produce NWI sample points within them
training_pts_func <- function(ids, areas = ny_areas, nwi = ny_nwi) {
    
    # The target area is the single ecoregion or area from within NY State
    target <- tidyterra::filter(areas, !!as.symbol(ID) == as.numeric(ids[[1]]))
    
    # target_name is for saving the files
    target_name <- ids[[1]]
    # print(target_name)
    
    # # target_area is the area dissolved/aggregated to a single polygon
    # target_area <- target |> 
    #     terra::aggregate()
    # target_area$US_L3NAME <- target_name
    # #print(names(target_area))
    
    # Reduce/crop the NWI wetlands to within target 
    nwi_area_crop <- terra::crop(x = nwi, y = target)
    # print(nwi_area_crop)
    
    # Reclassify wetlands in the cropped NWI to Forested, Emergent, or Scrub Shrub
    nwi_area_filter <- nwi_area_crop |> 
      tidyterra::mutate(WetClass = case_when(str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "ScrubShrub",
                                  str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "Emergent",
                                  str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "Forested",
                                  str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "ForestedScrub",
                                  str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "EmergentScrub",
                                  .default = ATTRIBUTE))

    # The number of different wetland polygons in the cropped NWI
    numEMW <- length(nwi_area_filter[nwi_area_filter$WetClass == "Emergent"])
    numFSW <- length(nwi_area_filter[nwi_area_filter$WetClass == "Forested"])
    numSSW <- length(nwi_area_filter[nwi_area_filter$WetClass == "ScrubShrub"])
    
    # The NWI wetland points are created by sampling 80% the NWI polygons
    nwi_pts_wet <- nwi_area_filter |>
        terra::buffer(-10) |> # a negative buffer should remove points on the lines
        terra::as.points() |> # turn the polygons into points
        sample(size = 0.80*(numEMW+numFSW+numSSW))  |> # 80% of the NWI points
        dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                            WetClass == "Forested" ~ "FSW",
                                            WetClass == "ScrubShrub" ~ "SSW",
                                            .default = "Other"),
                      COARSE_CLASS = "WET") |> # COARSE CLASS is for simple modeling
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    
    # Upland points are defined as outside the NWI polygons 
        # Might have some commission error/included wetlands, so there are many of these
      # Generate a random number of points in the area of interest
    pts <- terra::spatSample(target, method = "random", size = 5E5) 
      # reverse mask the random number of points outside NWI polygons
    nwi_pts_upl <- terra::mask(pts, nwi_area_crop |> buffer(10), inverse = TRUE) |>
        dplyr::mutate(MOD_CLASS = "UPL",
                      COARSE_CLASS = "UPL") |>
        dplyr::select(MOD_CLASS, COARSE_CLASS)
    
    # Combine upland and wetland points
    nwi_pts_all <- rbind(nwi_pts_upl, nwi_pts_wet)
    
    # The number of wetland points to balance the classes a bit
    numFSW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "FSW", ])
    numEMW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "EMW", ])
    numSSW_pts <- nrow(nwi_pts_all[nwi_pts_all$MOD_CLASS == "SSW", ])
    
    # If there are fewer than half of emergent and scrub/shrub vs. forested then supplement the points by sampling
        # additional emergent polygons
    if(numEMW_pts < 0.5*numFSW_pts & numSSW_pts < 0.5*numFSW_pts ){
        suppPointsEMW <- nwi_area_filter |>
            dplyr::filter(str_detect(WetClass, "Emergent")) |>
            terra::buffer(-10) |> # a negative buffer should remove points on the lines
            terra::as.points() |>
            sample(size = c(0.5*numFSW_pts, 1000)[which.max(c(0.5*numFSW_pts, 1000))])  |>
            dplyr::mutate(MOD_CLASS = "EMW",
                          COARSE_CLASS = "WET") |>
            dplyr::select(MOD_CLASS, COARSE_CLASS)
        suppPointsSSW <- nwi_area_filter |>
          dplyr::filter(str_detect(WetClass, "ScrubShrub")) |>
          terra::buffer(-10) |> # a negative buffer should remove points on the lines
          terra::as.points() |>
          sample(size = c(0.5*numFSW_pts, 1000)[which.max(c(0.5*numFSW_pts, 1000))])  |>
          dplyr::mutate(MOD_CLASS = "SSW",
                        COARSE_CLASS = "WET") |>
          dplyr::select(MOD_CLASS, COARSE_CLASS)
        
        nwi_pts_all_supp <- rbind(nwi_pts_all, suppPointsEMW, suppPointsSSW)
    } else { #don't change if > half of forested/scrub/shrub
        nwi_pts_all_supp <- nwi_pts_all
    }
    
    # Summary of point distribution
    print(nwi_pts_all_supp |> as.data.frame() |>  dplyr::group_by(MOD_CLASS) |> dplyr::summarise(count = n()))

    # Saving files for both the study area and the training data points 
    # writeVector(target, paste0("Data/NY_Ecoregions/",
    #                                 target_name,
    #                                #"_",
    #                                #ids,
    #                                "_ECO.gpkg"),
    #             overwrite = TRUE)
    writeVector(nwi_pts_all_supp, paste0("Data/Training_Data/",
                                         target_name,
                                         #"_",
                                         #ids,
                                         "_training_pts.gpkg"), overwrite = TRUE)
}

system.time({future_lapply(area_ids, training_pts_func, future.seed=TRUE)})

# test_nwi <- vect("Data/NWI/NY_Wetlands_6350.gpkg")
# test_areas <- vect("Data/ny_eco_l4/ny_eco_l4.shp") |> terra::project(crs(test_nwi))
# system.time(training_pts_func(ids = 59, nwi = test_nwi, areas = test_areas))
