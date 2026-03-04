l <- list.files("Data/TerrainProcessed/HUC_DEMs/", pattern = ".tif", full.names = TRUE)

extent_polys <- lapply(l, function(f) {
    r <- rast(f)
    as.polygons(ext(r), crs = crs(r))
})

all_extents <- Reduce(rbind, extent_polys)
all_extents$filename <- basename(list_of_huc_dems)
plet(all_extents, "filename", alpha = 0.4)


library(sf)
library(terra)
library(tidyverse)
nwi <- st_read("Data/NWI/NY_NWI_6347.gpkg") |> 
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
npmw <- st_read("Data/NYS_PrevMapWetlands/Previously_Mapped_Freshwater_Wetlands.shp")

clust <- st_read("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg") 
c208 <- clust[clust$cluster==208,]

nwi_208 <- st_filter(nwi |> st_transform(st_crs(c208)), c208)
npmw_208 <- st_filter(npmw |> st_transform(st_crs(c208)), c208)

plot(npmw_208 |> st_geometry())
plot(nwi_208 |> st_geometry(), add = T)

cmb_208 <- st_union(npmw_208, nwi_208)
