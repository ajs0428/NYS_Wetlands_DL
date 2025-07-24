library(terra)
library(sf)
library(tidyverse)
spec <- terra::vrt(list.files("Data/Satellite/",
                              pattern = ".img$|.tif",
                              full.names = TRUE))
v <- vect("Data/NY_HUCS/NY_HUCS_08_6350_Cluster.gpkg") #|>
#   tidyterra::filter(CLUSTER_ID == 11)  
  #terra::project(crs(spec))

sv <- st_read("Data/NY_HUCS/NY_HUCS_08_6350_Cluster.gpkg") |> 
  dplyr::filter(!is.na(geom))
centers <- sv |> 
  st_centroid() |> 
  dplyr::mutate(dplyr::as_tibble(st_coordinates(geom)))

km <- kmeans(centers |>
               st_drop_geometry() |>
               dplyr::select(X, Y), 
             centers = 10)
pts <- sv |> 
  dplyr::mutate(cluster = km$cluster)

############ Sentinel data
crp <- crop(spec, v)
plot(crp)
names(crp) <- names(rast("Data/Satellite/GEE_Asset0000000000-0000000000.tiff"))
crp
spec[1]
names(rast("Data/Satellite/GEE_Asset0000000000-0000000000.tiff"))

writeRaster(crp, "Data/Satellite/NYS_Clust11_spec.tif")


#############


nwi <- vect("Data/NWI/NY_Wetlands_6350.gpkg") |> 
  filter(!str_detect(ATTRIBUTE, "L1|L2|R1|R2|R3|R4|R5|E1|E2")) |> 
  filter(!str_detect(WETLAND_TY, "Pond|Marine|Lake|Other"))

nwi_filt <- nwi |> mutate(WetClass = case_when(str_detect(ATTRIBUTE, "PSS") & !str_detect(ATTRIBUTE, "PFO|PEM") ~ "ScrubShrub",
                                   str_detect(ATTRIBUTE, "PEM") & !str_detect(ATTRIBUTE, "PFO|PSS") ~ "Emergent",
                                   str_detect(ATTRIBUTE, "PFO") & !str_detect(ATTRIBUTE, "PSS|PEM") ~ "Forested",
                                   str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PFO") ~ "ForestedScrub",
                                   str_detect(ATTRIBUTE, "PSS") & str_detect(ATTRIBUTE, "PEM") ~ "EmergentScrub",
                                   .default = ATTRIBUTE))

nwi_filt |> as.data.frame() |> group_by(WetClass) |> summarise(n = n())

writeVector(nwi_filt, "Data/NWI/NY_Wetlands_6350_filterTrain.gpkg", overwrite =TRUE)
