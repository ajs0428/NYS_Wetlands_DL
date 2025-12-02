library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyterra))

################################################################################################
nhp_pts <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg",
                   quiet = TRUE, 
                   layer = "obspoints_attributed_systems_subsyst_cowardin") |> 
    sf::st_transform(crs = st_crs("EPSG:6347"))

nhp_poly <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg",
                    quiet = TRUE,
                    layer = "attributed_systems_subsyst_cowardin") |> 
    sf::st_transform(crs = st_crs("EPSG:6347"))

nhp_poly_statepark <- st_read("Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYNHP_NatComm_data_gpkg_20251120.gpkg",
                              quiet = TRUE,
                              layer = "parks_attributed_systems_subsyst_cowardin_sp") |> 
    sf::st_transform(crs = st_crs("EPSG:6347"))


nhp_polys_cmb <- bind_rows(nhp_poly, nhp_poly_statepark) |> 
    select(commname, system, cowardin) |> 
    filter(!str_detect(system, "Marine|Estuarine|Subterranean|Riverine")) |> 
    mutate(MOD_CLASS = case_when(str_detect(cowardin, "Palustrine-SS") ~ "SSW",
                                 str_detect(cowardin, "Palustrine-AB") ~ "OWW",
                                 str_detect(cowardin, "Open water") ~ "OWW",
                                 str_detect(cowardin, "Lacustrine") ~ "OWW",
                                 str_detect(cowardin, "Palustrine-EM") ~ "EMW",
                                 str_detect(cowardin, "Palustrine-FO") ~ "FSW",
                                 str_detect(cowardin, "Terrestrial") ~ "UPL",
                                 .default = "Other"),
           COARSE_CLASS = case_when(str_detect(cowardin, "Terrestrial") ~ "UPL",
                                    .default = "WET"),
           areas = as.numeric(st_area(geom)),
           num_pts = pmax(ceiling(log10(areas)), 1))

st_write(nhp_polys_cmb, "Data/NYS_NHP_Wetland_DelineatonData/NYNHP_NatComm_data/NYSWetlands_NYNHP_NatComm_data_combined.gpkg")

# EMW, FSW, SSW, OWW, UPL
unique(nhp_polys_cmb$MOD_CLASS)
nhp_polys_cmb |> group_by(cowardin) |> reframe(n = n())
nhp_polys_cmb |> group_by(MOD_CLASS) |> reframe(n = n())
nhp_polys_cmb |> group_by(COARSE_CLASS) |> reframe(n = n())

nhp_polys_to_points <- nhp_polys_cmb |> 
    st_sample(size = nhp_polys_cmb$num_pts) |> 
    st_sf() |>
    st_set_geometry("geom") |>
    st_join(nhp_polys_cmb[,c("MOD_CLASS", "COARSE_CLASS")])

nhp_points_cmb <- nhp_pts |> 
    select(commname, system, cowardin) |> 
    filter(!str_detect(system, "Marine|Estuarine|Subterranean|Riverine")) |> 
    mutate(MOD_CLASS = case_when(str_detect(cowardin, "Palustrine-SS") ~ "SSW",
                                 str_detect(cowardin, "Palustrine-AB") ~ "OWW",
                                 str_detect(cowardin, "Open water") ~ "OWW",
                                 str_detect(cowardin, "Lacustrine") ~ "OWW",
                                 str_detect(cowardin, "Palustrine-EM") ~ "EMW",
                                 str_detect(cowardin, "Palustrine-FO") ~ "FSW",
                                 str_detect(cowardin, "Terrestrial") ~ "UPL",
                                 .default = "UPL"),
           COARSE_CLASS = case_when(str_detect(cowardin, "Terrestrial") ~ "UPL",
                                    .default = "WET")) 
unique(nhp_points_cmb$MOD_CLASS)
nhp_points_cmb |> group_by(cowardin) |> reframe(n = n())
nhp_points_cmb |> group_by(MOD_CLASS) |> reframe(n = n())
nhp_points_cmb |> group_by(COARSE_CLASS) |> reframe(n = n())


bind_rows(nhp_polys_to_points, nhp_points_cmb |> select(MOD_CLASS, COARSE_CLASS, geom))



