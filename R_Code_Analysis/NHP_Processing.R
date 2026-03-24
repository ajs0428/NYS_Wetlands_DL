#### Reclassify and combine NHP wetland data with CHM relassified NWI data from Wetlands_CHM_reclass.R
#       Remove overlapping wetlands and prioritize keeping NHP

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyterra)
library(future)
library(future.apply)


set.seed(11)


########################################################################################

# args <- c(
#     64,
#     "Data/Training_Data/02_Done_Reviewed_NWI_Data/"
# )
# 
# args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here
# 
# cat("these are the arguments: \n", 
#     "1) Cluster number for HUC groups:", args[1], "\n", 
#     "2) path the reviewed training data :", args[2]
# )


setGDALconfig("GDAL_PAM_ENABLED", "FALSE") # does not create aux.xml files
########################################################################################
clusters <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE)

    # attributed_systems_subsyst_cowardin.
    # This is a collection of all recent (since year 2000) vegetation maps
    # completed by NYNHP from a wide variety of projects, from Federal, State, 
    # and private funders, with the exception of OPRHP State Parks, which are 
    # provided in a different layer. All polygons are attributed to NYNHP System, Subsystem, 
    # and Cowardin wetland type, as best as possible. In most cases, polygons 
    # from the element occurrence (EO) layer took precedence and polygons removed from 
    # this layer if they overlapped. Most cultural types (lawn, roads, developed land) were removed.
nhp_wetlands1 <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", 
                         layer = "attributed_systems_subsyst_cowardin", quiet = TRUE) |> 
    select(cowardin) |> 
    filter(!str_detect(cowardin, "Marine|Estuarine|Subterranean|Tidal")) |> # remove marine/estuarine
    mutate(MOD_CLASS = case_when(
        str_detect(cowardin, "Open water|Lacustrine|Riverine|Palustrine-AB") ~ "OWW",
        str_detect(cowardin, "Palustrine-SS") ~ "SSW",
        str_detect(cowardin, "Palustrine-EM") ~ "EMW",
        str_detect(cowardin, "Palustrine-FO") ~ "FSW",
        .default = "UPL"
    ))
    # eos_wetl_attributed_systems_subsyst_cowardin.
    # This layer contains wetland NYNHP Significant Natural Communities for 
    # New York State. Older, low-precision occurrences have been removed
    # and terrestrial natural communities have been removed. If desired, 
    # anyone can download the full set here:
    #     (https://data.gis.ny.gov/datasets/nysdec::natural-heritage-communities/about)
    # In a few cases, occurrences were also removed if delineations from 
    # one of the other two data sets overlapped and had a much better 
    # representation of the target community. In most cases, data were 
    # kept in this layer and removed from the other layers. 

nhp_wetlands2 <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", 
                         layer = "eos_wetl_attributed_systems_subsyst_cowardin", quiet = TRUE) |> 
    select(cowardin) |> 
    filter(!str_detect(cowardin, "Marine|Estuarine|Subterranean|Tidal")) |> # remove marine/estuarine
    mutate(MOD_CLASS = case_when(
        str_detect(cowardin, "Open water|Lacustrine|Riverine|Palustrine-AB") ~ "OWW",
        str_detect(cowardin, "Palustrine-SS") ~ "SSW",
        str_detect(cowardin, "Palustrine-EM") ~ "EMW",
        str_detect(cowardin, "Palustrine-FO") ~ "FSW",
        .default = "UPL"
    ))

    # parks_attributed_systems_subsyst_cowardin_sp.
    # This is a collection of vegetation maps for all New York State Parks. Attributes
    # for Cowardin types were added for this combining effort. If other layers
    # intersected these data, polygons were removed so that none overlapped. 
    # Most cultural types (lawn, roads, developed land) were removed.
    # Note that large matrix forests may have inclusions of wetlands
    # that fall below the minimum mapping unit of 1 acre. 

nhp_wetlands3 <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg", 
                         layer = "parks_attributed_systems_subsyst_cowardin_sp", quiet = TRUE) |> 
    select(cowardin) |> 
    filter(!str_detect(cowardin, "Marine|Estuarine|Subterranean|Tidal")) |> # remove marine/estuarine
    mutate(MOD_CLASS = case_when(
        str_detect(cowardin, "Open water|Lacustrine|Riverine|Palustrine-AB") ~ "OWW",
        str_detect(cowardin, "Palustrine-SS") ~ "SSW",
        str_detect(cowardin, "Palustrine-EM") ~ "EMW",
        str_detect(cowardin, "Palustrine-FO") ~ "FSW",
        .default = "UPL"
    ))

nhp_combine <- bind_rows(nhp_wetlands1, nhp_wetlands2, nhp_wetlands3) |> 
    st_transform(crs = st_crs("EPSG:6347"))
########################################################################################
clusters <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg")
nhp_huc_int <- st_intersects(clusters, nhp_combine, sparse = FALSE)
nhp_intersecting_hucs <- clusters[rowSums(nhp_huc_int) > 50, ]

########################################################################################
nwi_chm_rcl_list <- list.files("Data/Training_Data/Targeted_Wetlands_For_Field_Validation_v2/", 
                               full.names = T, pattern = ".gpkg")
nwi_chm_rcl_huc_list <- str_extract(nwi_chm_rcl_list, "(?<=huc_)\\d+")


########################################################################################
# Extract all NHP wetlands in a HUC 
    # Find matching CHM reclassified NWI wetlands in same HUC
    # Combine and remove overlapping polygons but keep NHP 

nhp_nwi_cmb_fun <- function(huc_num){
    cluster_num <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ][["cluster"]]
    fn <- paste0("Data/Training_Data/Targeted_Wetlands_For_Field_Validation_v3/NWI_CHM_reclass_TC_NHP_withReview_cluster_", cluster_num, "_huc_", huc_num, ".gpkg")
    
    if(!file.exists(fn)){
        huc <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ]
        nhp <- st_intersection(nhp_combine, huc) |> #NHP within test huc
            rename("WETLAND_TY" = "cowardin") |>
            select(WETLAND_TY, MOD_CLASS, huc12, cluster)
        nwi <- st_read(nwi_chm_rcl_list[grepl(pattern = huc_num, nwi_chm_rcl_list)]) |>
            select(WETLAND_TY, MOD_CLASS, huc12, cluster)

        nwi_int_nhp <- nwi[rowSums(st_intersects(nwi, nhp, sparse = F)) == 0,]
        cmb <- bind_rows(nhp, nwi_int_nhp)
        cmb$MOD_CLASS <- factor(cmb$MOD_CLASS, levels = c("EMW", "FSW", "OWW", "SSW", "UPL"))
        st_write(cmb, dsn = fn,
                 append = FALSE)
    } else {
        message(paste0("File for huc: ", huc_num, " already exists"))
    }
    
    return(NULL)
}

########################################################################################
# Extract all NHP wetlands in a HUC but do not add NWI 


nhp_singlehuc_fun <- function(huc_num){
    cluster_num <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ][["cluster"]]
    fn <- paste0("Data/Training_Data/NHP_HUC_Wetlands_For_Field_Validation/NHP_cluster_", cluster_num, "_huc_", huc_num, ".gpkg")
    
    if(!file.exists(fn)){
        huc <- clusters[grepl(pattern = huc_num, x = clusters$huc12), ]
        nhp <- st_intersection(nhp_combine, huc) |> #NHP within test huc
            rename("WETLAND_TY" = "cowardin") |>
            select(WETLAND_TY, MOD_CLASS, huc12, cluster)
        nhp$MOD_CLASS <- factor(nhp$MOD_CLASS, levels = c("EMW", "FSW", "OWW", "SSW", "UPL"))
        nhp <- st_cast(nhp, "POLYGON")
        if(nrow(nhp) > 0){
            st_write(nhp, dsn = fn,
                     append = FALSE)   
        } else {
            print("Zero NHP Features")
        }
    } else {
        message(paste0("File for huc: ", huc_num, " already exists"))
    }
    
    return(NULL)
}

########################################################################################



#### Single core/sequential 
# 
# lapply(nwi_chm_rcl_huc_list[1], nhp_nwi_cmb_fun)

lapply(nhp_intersecting_hucs$huc12, nhp_singlehuc_fun)

#### Parallel 
if(future::availableCores() > 16){
    corenum <-  4
} else {
    corenum <-  (future::availableCores())
}
print(corenum)
options(future.globals.maxSize= 32.0 * 1e9)
# plan(multisession, workers = corenum)
plan(future.callr::callr)

future_lapply(nhp_intersecting_hucs$huc12, nhp_singlehuc_fun, future.seed = TRUE, 
              future.packages = c("terra", "sf", "dplyr", "tidyr", "stringr"),
              future.globals = TRUE)
