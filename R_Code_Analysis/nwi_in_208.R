library(sf)
library(terra)

cluster_208 <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg") |> 
    filter(cluster == 208 & huc12 == "041402011002") |> 
    st_union() |> 
    st_transform("EPSG:6347")
nwi <- st_read("Data/NWI/NY_NWI_6347.gpkg", quiet = TRUE)

nwi_208 <- st_filter(nwi, cluster_208, .predicate = st_intersects) |> 
    st_intersection(cluster_208) |> 
    filter(WETLAND_TY != "Lake|Riverine")
nwi_208_filt <- nwi_208 |> 
    mutate(WetClass = case_when(
        str_detect(ATTRIBUTE, "L1|L2|PUB|PUS|PAB|R2|R3") & !str_detect(ATTRIBUTE, "PFO|PEM|PSS") ~ "OpenWater",
        str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "ScrubShrub",
        str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "Emergent",
        str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "Forested",
        str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "Forested",
        str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "Emergent",
        .default = ATTRIBUTE
    )) |> 
    dplyr::mutate(MOD_CLASS = case_when(WetClass == "Emergent" ~ "EMW", #MOD_CLASS is for modeling
                                        WetClass == "Forested" ~ "FSW",
                                        WetClass == "ScrubShrub" ~ "SSW",
                                        WetClass == "OpenWater" ~ "OWW",
                                        WetClass == "UPL" ~ "UPL",
                                        .default = "Other")) |> 
    filter(WETLAND_TY != "Lake", WETLAND_TY != "Riverine" ) |> 
    select(MOD_CLASS, geom)

plot((nwi_208_filt["MOD_CLASS"]))

st_write(nwi_208_filt, append = FALSE,
         "Data/Training_Data/HUC_Extracted_Training_Data/cluster_208_huc_041402011002_NWI.gpkg")
