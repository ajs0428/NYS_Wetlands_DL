#!/usr/bin/env Rscript

args <- c(
    "Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYSWetlands_NYNHP_NatComm_data_combined.gpkg", # 
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg", # 
    "cowardin", # 
    "cluster" # 
)

args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) The wetland dataset to sample from for training data:", args[1], "\n", 
    "2) path to the overall zones or study areas :", args[2], "\n",
    "3) The data field from the wetland dataset with sample labels:", args[3], "\n",
    "4) The data field within the zones or study areas file :", args[4], "\n")

###############################################################################################

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)


set.seed(11)

###############################################################################################
# filter the NWI down to the targeted wetlands
wetlands <- st_read(args[1], quiet = TRUE) 

if(st_crs(wetlands) != st_crs("EPSG:6347")){
    wetlands <- st_transform(wetlands, "EPSG:6347")
    st_write(wetlands, paste0(str_remove(args[1], "\\..*"), "_6347", ".gpkg"), delete_layer = TRUE)
} else {
    print("No reprojection to EPSG:6347")
}
    
# The areas of interest are projected to match the NWI
ny_areas <- st_read(args[2], quiet = TRUE)

if(st_crs(ny_areas) != st_crs("EPSG:6347")){
    print("Needs reprojection to EPSG:6347")
    ny_areas <- st_transform(ny_areas, "EPSG:6347")
    #st_write(wetlands, paste0(str_remove(args[2], "\\..*"), "_6347", ".gpkg"), delete_layer = TRUE)
} else {
    print("No reprojection to EPSG:6347")
}

# The list of area names/IDs
ID <- args[4]
area_ids <- ny_areas[, ID][[1]] |> na.omit() |> unique()

###############################################################################################
# The function should take a arguments to subset ecoregions/study areas and produce NWI sample points within them
    #### Separate surface water to be used to filter out unwanted poins 
    #### Generate training data from wetlands but filter upland points by full NWI
training_pts_func <- function(ids, areas = ny_areas, fullwetlands = wetlands) {
    #print(fullwetlands)
    # The target area is the single zone or area from within NY State
    target <- filter(areas, !!as.symbol(args[4]) == ids[1])
    #print(target)
    
    # target_name is for saving the files
    target_name <- ids[[1]]
    # print(target_name)
    
    wetlands_target <- st_filter(fullwetlands, target, .predicate = st_within)
    
    which_wetlands <- str_extract(basename(args[3]), regex("WETLAND_TY|cowardin|comm|ATTRIBUTE", ignore_case = TRUE))
    if(which_wetlands %in% c("WETLAND_TY","ATTRIBUTE")){
        wetlands_name <- "NWI"
    } else if(which_wetlands %in% c("cowardin","comm")){
        wetlands_name <- "DEC_NHP"
    }
    # Reclassify wetlands to Forested, Emergent, or Scrub Shrub
    if(wetlands_name == "NWI"){
        wetlands_area_filter <- wetlands_target |> 
            filter(!str_detect(ATTRIBUTE, "L1|R1|R4|R5")) |> # remove big lake and small streams (unreliable)
            filter(!str_detect(WETLAND_TY, "Marine|Estuarine|Other")) |> # remove marine/estuarine
            mutate(WetClass = case_when(
                str_detect(ATTRIBUTE, "L2|PUB|PUS|PAB|R2|R3") & !str_detect(ATTRIBUTE, "PFO|PEM|PSS") ~ "OpenWater",
                str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "ScrubShrub",
                str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "Emergent",
                str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "Forested",
                str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "Forested",
                str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "Emergent",
                .default = ATTRIBUTE
            ))
    } else if(wetlands_name == "DEC_NHP"){
        wetlands_area_filter <- wetlands_target |> 
            filter(!str_detect(system, "Marine|Estuarine|Subterranean|Riverine")) |> 
            mutate(WetClass = case_when(str_detect(cowardin, "Palustrine-SS") ~ "ScrubShrub",
                                         str_detect(cowardin, "Palustrine-AB") ~ "OpenWater",
                                         str_detect(cowardin, "Open water") ~ "OpenWater",
                                         str_detect(cowardin, "Lacustrine") ~ "OpenWater",
                                         str_detect(cowardin, "Palustrine-EM") ~ "Emergent",
                                         str_detect(cowardin, "Palustrine-FO") ~ "Forested",
                                         str_detect(cowardin, "Terrestrial") ~ "UPL",
                                         .default = "Other"),
                   Shape_Area = as.numeric(st_area(geom)))
    }
    
    print(wetlands_area_filter |> as_tibble() |> reframe(n = unique(WetClass)))
    # The number of different wetland polygons in the cropped target areas
    numEMW <- nrow(wetlands_area_filter[wetlands_area_filter$WetClass == "Emergent",])
    numFSW <- nrow(wetlands_area_filter[wetlands_area_filter$WetClass == "Forested",])
    numSSW <- nrow(wetlands_area_filter[wetlands_area_filter$WetClass == "ScrubShrub",])
    numOWW <- nrow(wetlands_area_filter[wetlands_area_filter$WetClass == "OpenWater",])
    numUPL <- nrow(wetlands_area_filter[wetlands_area_filter$WetClass == "UPL",])
    print(paste0("emergent: ",numEMW))
    print(paste0("forest: ",numFSW))
    print(paste0("shrub: ",numSSW))
    print(paste0("open water: ",numOWW))
    
    tryCatch({
        # The wetland points are created turning polygons to points
        wetlands_pts_wet <- wetlands_area_filter |>
            dplyr::mutate(geom = case_when(Shape_Area > 3000 ~ st_buffer(geom, -10), # a negative buffer should remove points on the lines
                                           .default = geom)) %>% # but keep area > 3000 to still sample small wetlands
            filter(!st_is_empty(.)) |>
            st_sample(size = sum(numEMW, numFSW, numSSW,numOWW, na.rm = TRUE)*1.5) |> # The number of points for wetlands is equal to the number of wetlands x2
            st_sf() |>
            st_set_geometry("geom") |>
            st_join(wetlands_area_filter[,"WetClass"]) |>
            dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                                WetClass == "Forested" ~ "FSW",
                                                WetClass == "ScrubShrub" ~ "SSW",
                                                WetClass == "OpenWater" ~ "OWW",
                                                WetClass == "UPL" ~ "UPL",
                                                .default = "Other"),
                          COARSE_CLASS = case_when(WetClass == "UPL" ~ "UPL",
                                                   .default = "WET")) |> # COARSE CLASS is for simple modeling
            dplyr::select(MOD_CLASS, COARSE_CLASS)
        print(wetlands_pts_wet)
        
        # Upland points are defined as outside the wetland polygons
        # Might have some commission error/included wetlands, so there are many of these
        
        pts <- st_sample((target), size = sum(numEMW, numFSW, numSSW, numOWW, na.rm = TRUE)*10) # Setting number of upland points to 10x the number of wetland points
        # reverse mask the random number of points outside NWI polygons
        if(wetlands_name == "NWI"){
            wetlands_pts_upl <- pts[lengths(st_intersects(pts, st_geometry(wetlands_target) |>
                                                              st_buffer(100), sparse = TRUE)) == 0,] |>
                st_sf() |>
                st_set_geometry("geom") |>
                dplyr::mutate(MOD_CLASS = "UPL",
                              COARSE_CLASS = "UPL") |>
                dplyr::select(MOD_CLASS, COARSE_CLASS)
            # print(wetlands_pts_upl)
            # Combine upland and wetland points
            wetlands_pts_all <- rbind(wetlands_pts_upl, wetlands_pts_wet)
        } else if(wetlands_name == "DEC_NHP"){
            wetlands_pts_upl <- wetlands_area_filter |> 
                filter(WetClass == "UPL")  |>
                dplyr::mutate(geom = case_when(Shape_Area > 3000 ~ st_buffer(geom, -10), # a negative buffer should remove points on the lines
                                               .default = geom)) %>% # but keep area > 3000 to still sample small wetlands
                filter(!st_is_empty(.)) |>
                st_sample(size = sum(numUPL, na.rm = TRUE)*2) |>
                st_sf() |>
                st_set_geometry("geom") |>
                st_join(wetlands_area_filter[,"WetClass"]) |>
                dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                                    WetClass == "Forested" ~ "FSW",
                                                    WetClass == "ScrubShrub" ~ "SSW",
                                                    WetClass == "OpenWater" ~ "OWW",
                                                    WetClass == "UPL" ~ "UPL",
                                                    .default = "Other"),
                              COARSE_CLASS = case_when(WetClass == "UPL" ~ "UPL",
                                                       .default = "WET")) |> # COARSE CLASS is for simple modeling
                dplyr::select(MOD_CLASS, COARSE_CLASS)
            wetlands_pts_all <- rbind(wetlands_pts_upl, wetlands_pts_wet)
        }
        
        
        # The number of wetland points to balance the classes a bit
        numFSW_pts <- nrow(wetlands_pts_all[wetlands_pts_all$MOD_CLASS == "FSW", ])
        numEMW_pts <- nrow(wetlands_pts_all[wetlands_pts_all$MOD_CLASS == "EMW", ])
        numSSW_pts <- nrow(wetlands_pts_all[wetlands_pts_all$MOD_CLASS == "SSW", ])
        numOWW_pts <- nrow(wetlands_pts_all[wetlands_pts_all$MOD_CLASS == "OWW", ])
        numUPL_pts <- nrow(wetlands_pts_all[wetlands_pts_all$MOD_CLASS == "UPL", ])
        print(paste0("numFSW_pts:", numFSW_pts))
        print(paste0("numEMW_pts:", numEMW_pts))
        print(paste0("numSSW_pts:", numSSW_pts))
        print(paste0("numOWW_pts:", numOWW_pts))
        print(paste0("numUPL_pts:", numUPL_pts))
        # If there are fewer than half of emergent and scrub/shrub vs. forested then supplement the points by sampling
        # additional emergent polygons
        if(numEMW_pts < 0.5*numFSW_pts & numSSW_pts < 0.5*numFSW_pts ){
            suppPointsEMW <- wetlands_pts_all |>
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
            suppPointsSSW <- wetlands_area_filter |>
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
            
            wetlands_pts_all_supp <- rbind(wetlands_pts_all, suppPointsEMW, suppPointsSSW)
        } else { #don't change if > half of forested/scrub/shrub
            wetlands_pts_all_supp <- wetlands_pts_all
        }
        
        if(sum(numFSW_pts,numEMW_pts,numSSW_pts) < 0.50*numOWW_pts){ #open water might be biased towards big lakes
            suppPointsAll <- wetlands_pts_all |>
                dplyr::filter(!str_detect(WetClass, "OpenWater")) |> #select all non-open water
                # dplyr::filter(Shape_Area < 5000) |>  #try to sample smaller wetlands that might have been missed
                st_buffer(-1) %>% # a smaller negative buffer should remove points on the lines
                filter(!st_is_empty(.)) |>
                st_sample(size = ceiling(numOWW_pts/2)) |> # increase by 50%?
                st_sf() |>
                st_set_geometry("geom") |>
                st_join(wetlands_pts_all[,"WetClass"]) |>
                dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                                    WetClass == "Forested" ~ "FSW",
                                                    WetClass == "ScrubShrub" ~ "SSW",
                                                    WetClass == "OpenWater" ~ "OWW",
                                                    .default = "Other"),
                              COARSE_CLASS = "WET") |> # COARSE CLASS is for simple modeling
                dplyr::select(MOD_CLASS, COARSE_CLASS)
            wetlands_pts_all_supp2 <- rbind(wetlands_pts_all_supp, suppPointsAll)
        } else {
            wetlands_pts_all_supp2 <- wetlands_pts_all_supp
        }
        
        # Summary of point distribution
        print("final point summary")
        print(wetlands_pts_all_supp2 |> as.data.frame() |>  dplyr::group_by(MOD_CLASS) |> dplyr::summarise(count = n()))
        
        st_write(obj = wetlands_pts_all_supp2, dsn = paste0("Data/Training_Data/",
                                                            args[4],"_",
                                                            target_name,
                                                            "_",
                                                            wetlands_name,
                                                            "_training_pts.gpkg"), append = FALSE, delete_layer = TRUE)
        
        gc(verbose = FALSE)
        return(invisible(NULL))
    }, error = function(e){
        message("Error, probably 0 wetland polygons: ", e$message)
        return(NA)
    })
   
}

###############################################################################################
if(future::availableCores() > 16){
    corenum <-  8
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 16 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

# this should probably be an argument for bash
system.time({future_lapply(area_ids, training_pts_func, future.seed=TRUE)})


# Single cluster
# lapply(area_ids[area_ids %in% c(168)], training_pts_func)
