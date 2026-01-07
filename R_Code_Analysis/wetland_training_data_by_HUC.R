### This script is meant to be run on a remote cluster 

library(terra)
library(sf)
library(tidyverse)

l <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".tif", full.names = TRUE) 
lf <- l[str_detect(l, "cluster_208") & !str_detect(l, "wbt")]

args <- c(
    "Data/NWI/NY_NWI_6347.gpkg", 
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg", # 
    208 # 
)
########################################################################################
wetlands <- st_read(args[1], quiet = TRUE) 

if(st_crs(wetlands) != st_crs("EPSG:6347")){
    wetlands <- st_transform(wetlands, "EPSG:6347")
    st_write(wetlands, paste0(str_remove(args[1], "\\..*"), "_6347", ".gpkg"), delete_layer = TRUE)
} else {
    print("No reprojection to EPSG:6347")
}

wetlands_filter <- wetlands |> 
    filter(!str_detect(ATTRIBUTE, "R1|R4|R5")) |> # remove and small streams (unreliable)
    filter(!str_detect(WETLAND_TY, "Marine|Estuarine|Other")) |> # remove marine/estuarine
    mutate(MOD_CLASS = case_when(
        str_detect(ATTRIBUTE, "L1|L2|PUB|PUS|PAB|R2|R3") & !str_detect(ATTRIBUTE, "PFO|PEM|PSS") ~ "OWW",
        str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "SSW",
        str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "EMW",
        str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "FSW",
        str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "FSW",
        str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "EMW",
        .default = ATTRIBUTE
    ))

########################################################################################
ny_areas <- st_read(args[2], quiet = TRUE, query = "SELECT * FROM \"NY_Cluster_Zones_250_NAomit\" WHERE cluster = 208")

if(st_crs(ny_areas) != st_crs("EPSG:6347")){
    print("Needs reprojection to EPSG:6347")
    ny_areas <- st_transform(ny_areas, "EPSG:6347")
    st_write(ny_areas, paste0(str_remove(args[2], "\\..*"), "_6347", "_cluster_208_", ".gpkg"), delete_layer = TRUE)
} else {
    print("No reprojection to EPSG:6347")
}

ny_areas_union <- st_union(ny_areas)

########################################################################################
nhp_wetlands1 <- st_read("FieldData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", layer = "attributed_systems_subsyst_cowardin") |> 
    select(cowardin)
nhp_wetlands2 <- st_read("FieldData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", layer = "eos_wetl_attributed_systems_subsyst_cowardin") |> 
    select(cowardin)
nhp_wetlands3 <- st_read("FieldData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", layer = "parks_attributed_systems_subsyst_cowardin_sp") |> 
    select(cowardin)

nhp_wetlands <- rbind(nhp_wetlands1, nhp_wetlands2, nhp_wetlands3)

if(st_crs(nhp_wetlands) != st_crs("EPSG:6347")){
    print("Needs reprojection to EPSG:6347")
    nhp_wetlands <- st_transform(nhp_wetlands, "EPSG:6347")
} else {
    print("No reprojection to EPSG:6347")
}

nhp_wetlands_filter <- nhp_wetlands |> 
    filter(!str_detect(cowardin, "Marine|Estuarine|Terrestrial|Subterranean|Tidal")) |> # remove marine/estuarine
    mutate(MOD_CLASS = case_when(
        str_detect(cowardin, "Open water|Lacustrine|Riverine|Palustrine-AB") ~ "OWW",
        str_detect(cowardin, "Palustrine-SS") ~ "SSW",
        str_detect(cowardin, "Palustrine-EM") ~ "EMW",
        str_detect(cowardin, "Palustrine-FO") ~ "FSW"
    )) 
unique(nhp_wetlands_filter$MOD_CLASS)
########################################################################################

wetlands_filter_inarea <- st_intersection(wetlands_filter, ny_areas_union)

nhp_wetlands_inarea <- st_intersection(nhp_wetlands_filter, ny_areas_union)

nhp_wetlands_overlap <- st_intersects(wetlands_filter_inarea, nhp_wetlands_inarea)

overlap_list <- which(lengths(nhp_wetlands_overlap) > 0)

wetlands_nhpfilter_inarea <- wetlands_filter_inarea[-overlap_list, ]

wetlands_nhp_cmb_inarea <- wetlands_nhpfilter_inarea |> 
    select(MOD_CLASS) |>
    rbind(nhp_wetlands_inarea |> select(MOD_CLASS))

ggplot()  + 
    geom_sf(data = ny_areas_union) + 
    geom_sf(data = wetlands_nhp_cmb_inarea, aes(fill = MOD_CLASS))

st_write(wetlands_nhp_cmb_inarea, "Data/Training_Data/Wetland_Polygons_For_DL/NHP_NWI_cluster_208_MOD_CLASS.gpkg",
         append = FALSE)

########################################################################################

list_of_hucs <- ny_areas[["huc12"]]

wetland_extract_fun <- function(huc_num){
    single_huc <- ny_areas[ny_areas$huc12 == huc_num,]
    wetlands_in_huc <- st_intersection(wetlands_nhp_cmb_inarea, single_huc)
    filename <- (paste0("Data/Training_Data/Wetland_Polygons_For_DL/NHP_NWI_cluster_", args[3], "_huc_", huc_num, "_MOD_CLASS_wetlands.gpkg"))
    st_write(wetlands_in_huc,
             dsn = filename)
}

lapply(list_of_hucs, wetland_extract_fun)
