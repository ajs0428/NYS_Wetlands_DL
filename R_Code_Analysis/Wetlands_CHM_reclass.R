### NWI Reclassification based on CHM 

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
    208, #Target Cluster
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", #Clusters and HUCs
    "Data/Tompkins County Wetland Mapping 2015/Geospatial Data/Tompkins County Wetlands 2012 SHP/Tompkins County Wetlands 2012.shp", # Wetlands
    "NWCS_Type" # Field for filtering and matching
    )


args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "1) Cluster number for HUC groups:", args[1], "\n", 
    "2) path to the overall zones or study areas :", args[2], "\n",
    "3) Path to the wetlands:", args[3], "\n",
    "4) Field in polygons for filtering and extracting wetland type: ", args[4], "\n"
    )

########################################################################################
l_chm <- list.files("Data/CHMs/HUC_CHMs/", pattern = ".tif", full.names = TRUE) 
l_chm_cluster <- l_chm[str_detect(l_chm, paste0("cluster_", args[1]))] # CHMs for args[1] cluster of HUCs 
l_chm_cluster_nums <- str_extract(l_chm, "(?<=cluster_)\\d+(?=_)") |> unique() # each cluster of HUCs present in the CHMs 

########################################################################################
# all the HUCs in the entire CHM dataset
ny_hucs <- sf::st_read(args[2], quiet = TRUE,
                       query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE cluster IN (", 
                                      paste(l_chm_cluster_nums, collapse = ","), ")")) 
# The HUCs in the cluster from args[1]
huc_cluster <- sf::st_read(args[2], quiet = TRUE,
                       query = paste0("SELECT * FROM NY_Cluster_Zones_250_NAomit_6347 WHERE cluster IN (", args[1], ")"))
huc_nums_cluster <- huc_cluster$huc12

print(huc_nums_cluster)
print(huc_cluster)
########################################################################################

# All wetlands from NWI and other sources??
wetlands <- st_read(args[3], quiet = F) 

if(st_crs(wetlands) != st_crs("EPSG:6347")){
    wetlands <- st_transform(wetlands, "EPSG:6347")
    st_write(wetlands, paste0(str_remove(args[3], "\\..*"), "_6347", ".gpkg"), delete_layer = TRUE)
} else {
    print("No reprojection to EPSG:6347")
}

wetlands_filter <- wetlands |>
    filter(!str_detect(.data[[args[4]]], "^R1|^R3|^R4|^R5|^E1|^E2|^E3|^M1|^M2|^M3|^Other"))# remove and small streams (unreliable) # remove marine/estuarine

# This gives it the "huc" and "cluster" fields, conveniently 
wetlands_cluster <- st_intersection(wetlands_filter, huc_cluster) |> vect() |> terra::wrap()

########################################################################################

#For each HUC watershed
    # mask and extract CHM using the NWI wetlands
    # Zonal stats appended to NWI wetlands
    # Reclassify based on Mahoney et al., 2022 (Colin Beier) 1m ≤ Shrubland ≤5m

wetland_chm_extract_classify <- function(huc_num){
    
    tryCatch({
        r_chm <- rast(l_chm_cluster[str_detect(l_chm_cluster , huc_num)])
        v_wet <- terra::unwrap(wetlands_cluster)
        v_wet <- v_wet[v_wet$huc12 == huc_num]
        og_name <- tools::file_path_sans_ext(basename(args[3])) |> gsub(pattern = " ", replacement = "")

        filename <- paste0("Data/Training_Data/TompkinsCounty_Reclass/", og_name, "_cluster_", args[1], "_huc_", huc_num, ".gpkg")
        
        if(!file.exists(filename)){
            message(paste0("Creating New Wetlands Reclass File: ", filename))
            wet_chm <- terra::extract(r_chm, v_wet, "mean", bind = TRUE) |> 
                tidyterra::mutate(
                    MOD_CLASS = dplyr::case_when(
                        str_detect(.data[[args[4]]], "L1|L2|PUB|PUS|PAB|R2|R3") & !str_detect(.data[[args[4]]], "PFO|PEM|PSS") & CHM <= 1.0 ~ "OWW",
                        # str_detect(.data[[args[4]]], "L1|L2|PUB|PUS|PAB|R2|R3") & !str_detect(.data[[args[4]]], "PFO|PEM|PSS") & CHM > 1.0 & CHM <= 3.5 ~ "EMW",
                        str_detect(.data[[args[4]]], "PSS") & !str_detect(.data[[args[4]]], "FO|EM") & CHM >= 1.0 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PSS") & !str_detect(.data[[args[4]]], "FO|EM") & CHM > 5.0 ~ "FSW",
                        str_detect(.data[[args[4]]], "PEM") & !str_detect(.data[[args[4]]], "FO|SS") & CHM <= 3.5 ~ "EMW",
                        str_detect(.data[[args[4]]], "PEM") & !str_detect(.data[[args[4]]], "FO|SS") & CHM >= 1.0 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PEM") & !str_detect(.data[[args[4]]], "FO|SS") & CHM > 5.0 ~ "FSW",
                        str_detect(.data[[args[4]]], "PFO") & !str_detect(.data[[args[4]]], "SS|EM") & CHM >= 1.0 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PFO") & !str_detect(.data[[args[4]]], "SS|EM") & CHM > 5.0 ~ "FSW",
                        str_detect(.data[[args[4]]], "PFO") & str_detect(.data[[args[4]]], "SS|EM") & CHM <= 3.5 ~ "EMW",
                        str_detect(.data[[args[4]]], "PFO") & str_detect(.data[[args[4]]], "SS|EM") & CHM >= 1.0 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PFO") & str_detect(.data[[args[4]]], "SS|EM") & CHM > 5.0 ~ "FSW",
                        str_detect(.data[[args[4]]], "PSS") & str_detect(.data[[args[4]]], "FO") & CHM >= 1.0 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PSS") & str_detect(.data[[args[4]]], "FO") & CHM > 5.0 ~ "FSW",
                        str_detect(.data[[args[4]]], "PSS") & str_detect(.data[[args[4]]], "EM") & CHM <= 3.5 ~ "EMW",
                        str_detect(.data[[args[4]]], "PSS") & str_detect(.data[[args[4]]], "EM") & CHM > 3.5 & CHM <= 5.0 ~ "SSW",
                        str_detect(.data[[args[4]]], "PEM") & str_detect(.data[[args[4]]], "SS") & CHM <= 3.5 ~ "EMW",
                        str_detect(.data[[args[4]]], "PEM") & str_detect(.data[[args[4]]], "SS") & CHM > 3.5 & CHM <= 5.0 ~ "SSW",
                        .default = "REVIEW"
                    ))
            print(unique(wet_chm$MOD_CLASS))
            
            writeVector(wet_chm, filename = filename, overwrite = TRUE)
            rm(r_chm)
            rm(v_wet)
            rm(wet_chm)
            # return(wet_chm)
        } else {
            message(paste0("NWI Reclass File Aleady Exists: ", filename))
        }
    }, error = function(e){
        message(e$message)
        return(NA)
    })
    return(invisible(NULL))
    gc()
}



###############################################################################################
if(future::availableCores() > 16){
    corenum <-  8
} else {
    corenum <-  (future::availableCores())
}
options(future.globals.maxSize= 16 * 1e9)

plan(future.callr::callr, workers = corenum)

system.time({future_lapply(huc_nums_cluster, 
                           wetland_chm_extract_classify, 
                           future.seed=TRUE,
                           future.packages = c("terra", "sf", "tidyverse", "future", "future.lapply"),
                           future.globals = TRUE)})

# lapply(huc_nums_cluster, wetland_chm_extract_classify)

# rm(wetlands)
# rm(wetlands_filter)