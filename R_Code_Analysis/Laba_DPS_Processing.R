### Laba/DPS Reclassification based on CHM & NAIP

library(terra)
library(sf) 
library(tidyverse)
library(stringr)
library(tidyterra)
library(future)
library(future.apply)


set.seed(11)

########################################################################################

args <- c(
    "Data/Laba_NYS_Info_Wetlands/Laba_94C_Wetland_Delineations_20240210_Valid.gpkg" # Wetlands
)


args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) Path to the wetlands:", args[1], "\n"
)

########################################################################################

clusters <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE)

########################################################################################
l_chm <- list.files("Data/CHMs/HUC_CHMs/", pattern = ".tif", full.names = TRUE) 
l_chm_huc_nums <- str_extract(l_chm, "(?<=huc_)\\d+(?=_)") |> unique() # each HUC present in the CHMs 

l_naip <- list.files("Data/NAIP/HUC_NAIP_Processed/", pattern = ".tif", full.names = TRUE) 

########################################################################################

LabaWetlands <- st_read(args[1], quiet = TRUE) |> 
    dplyr::select(geom)

# LabaWetlandsValid <- LabaWetlands |> filter(st_is_valid(LabaWetlands) ) |> 
#     st_transform("EPSG:6347") %>%
#     filter(!is.na(st_is_valid(.)))
# 
# st_write(LabaWetlandsValid, "Data/Laba_NYS_Info_Wetlands/Laba_94C_Wetland_Delineations_20240210_Valid.gpkg")

LabaHUC12 <- clusters[rowSums(st_intersects(clusters, LabaWetlands, sparse = FALSE)) > 0, ][["huc12"]]

laba_wetland_chm_extract_classify <- function(huc_num){

    tryCatch({
        if(sum(length(l_naip[str_detect(l_naip , huc_num)]), 
               length(l_chm[str_detect(l_chm , huc_num)])) > 1 ){
            print("CHM and NAIP Files Exist")
            r_chm <- rast(l_chm[str_detect(l_chm , huc_num)])
            r_naip <- rast(l_naip[str_detect(l_naip , huc_num)])
            stack <- c(r_chm, r_naip)
            huc <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ]
            cluster_num <-  clusters[grepl(pattern = huc_num, x = clusters$huc12), ][["cluster"]]

            filename <- paste0("Data/Training_Data/HUC_Laba_Processed/Laba_NYS_Wetlands_cluster_", cluster_num, "_huc_", huc_num, ".gpkg")

            if(!file.exists(filename)){
                message(paste0("Creating New Laba Reclass File: ", filename))
                laba_huc <- st_intersection(LabaWetlands, huc)  |> vect()
                wet_chm <- terra::extract(stack, laba_huc, fun = "median", bind = TRUE) |>
                    tidyterra::mutate(
                        MOD_CLASS = dplyr::case_when(
                            CHM <= 0.5 & ndwi > 0.2 ~ "OWW",
                            CHM > 0.5 & CHM <= 3.5 & ndwi > 0.2 ~ "EMW",
                            CHM >= 0.5 & CHM <= 5.0 & ndwi < 0.2~ "SSW",
                            CHM > 5.0 ~ "FSW",
                            CHM <= 3.5 ~ "EMW",
                            CHM > 3.5 & CHM <= 5.0 ~ "SSW",
                            .default = "REVIEW"
                        )) |> 
                    dplyr::select(MOD_CLASS)

                print(wet_chm)
                writeVector(wet_chm, filename = filename, overwrite = TRUE)
                rm(wet_chm)
                #return(wet_chm)
            } else {
                message(paste0("NWI Reclass File Aleady Exists: ", filename))
            }
        } else {
            print("No Files")
        }
        
    }, error = function(e){
        message(e$message)
        return(NA)
    })
    return(invisible(NULL))
    gc()
}

t <- lapply(LabaHUC12, laba_wetland_chm_extract_classify)


### Testing 
# system.time({test <- laba_wetland_chm_extract_classify("043002010502")})
# 
# plet(test, "MOD_CLASS")
# 

