### Replace old NWI with new Tompkins County wetland map

# Nicholas Hollingshead 2015


library(sf)
library(dplyr)
library(stringr)

ny_nwi <- st_read("Data/NWI/NY_NWI_6347.gpkg")
l_nwi <- list.files("Data/Training_Data/Targeted_Wetlands_For_Field_Validation/", 
                    pattern = "NWI_CHM_reclas", full.names = TRUE)
all_nwi_chm <- lapply(l_nwi, st_read, quiet = TRUE) |> bind_rows()
ny_clusters <- st_read("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit_6347.gpkg", quiet = TRUE)
tc <- st_read("Data/Boundaries/tompkins_county.gpkg", quiet = TRUE) |> st_cast("MULTIPOLYGON") |> st_buffer(1000, endCapStyle = "SQUARE")
tc_wetlands <- st_read("Data/Tompkins County Wetland Mapping 2015/Geospatial Data/Tompkins County Wetlands 2012 SHP/Tompkins County Wetlands 2012.shp") |> 
    st_transform("EPSG:6347")
ny_nwi_tc <- st_intersection(ny_nwi, tc)

tc_clusters <- st_join(tc_wetlands, ny_clusters |> select(huc12, cluster)) |> 
    filter(!str_detect(NWCS_Type, "R1|R4|R5")) |> # remove and small streams (unreliable)
    filter(!str_detect(Class_Prim, "Marine|Estuarine|Other")) |> # remove marine/estuarine
    mutate(
        MOD_CLASS = case_when(
        str_detect(NWCS_Type, "L1|L2|PUB|PUS|PAB|R2|R3") & !str_detect(NWCS_Type, "PFO|PEM|PSS") ~ "OWW",
        str_detect(NWCS_Type, "PSS") & !str_detect(NWCS_Type, "PFO|PEM") ~ "SSW",
        str_detect(NWCS_Type, "PEM") & !str_detect(NWCS_Type, "PFO|PSS") ~ "EMW",
        str_detect(NWCS_Type, "PFO") & !str_detect(NWCS_Type, "PSS|PEM") ~ "FSW",
        str_detect(NWCS_Type, "PSS") & str_detect(NWCS_Type, "FO") ~ "SSW", #Change here because SSW is confused with FSW
        str_detect(NWCS_Type, "PSS") & str_detect(NWCS_Type, "PEM") ~ "EMW",
        .default = NWCS_Type
        )
    ) |>
    select(cluster, huc12, MOD_CLASS)
st_geometry(tc_clusters) <- "geom"

tc_nwi_int <- st_intersects(all_nwi_chm, tc_clusters, sparse = FALSE) # the NWI that intersects TC wetlands
nwi_chm_noTC <- all_nwi_chm[rowSums(tc_nwi_int) == 0,] # NWI with no intersections

# ny_nwi_tc_rep <- all_nwi_chm |> st_difference(st_union(tc_clusters))

updated_ny_nwi_tc <- bind_rows(nwi_chm_noTC, tc_clusters) # bind NWI no TC with TC

updated_ny_nwi_tc[is.na(updated_ny_nwi_tc$huc12),]

st_write(updated_ny_nwi_tc, "Data/Training_Data/NWI_CHM_reclass_TompkinsCounty.gpkg",
         append = FALSE)

for(i in sort(unique(updated_ny_nwi_tc$huc12))){
    w <- updated_ny_nwi_tc[updated_ny_nwi_tc$huc12 == i, ]
    cl <- first(w$cluster)
    st_write(w,
             print(paste0("Data/Training_Data/Targeted_Wetlands_For_Field_Validation_v2/",
                           "NWI_CHM_reclass_TC_", "withReview_cluster_", 
                           cl, "_huc_", i, ".gpkg")),
             append = FALSE)
}
