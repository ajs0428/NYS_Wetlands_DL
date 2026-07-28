library(terra)
library(sf)

m_class <- rast(
  "Data/HUC_DL_Predictions/DLpred_multiclass_cluster_208_huc_041402011103.tif"
)

# cell area in m² from the projected resolution (1 x 1 = 1 m² here)
cell_area <- prod(res(m_class))

# count pixels per class (fast, streams over the raster)
f <- freq(m_class)

# f has columns: layer, value, count
f$area_m2 <- f$count * cell_area
f$area_ha <- f$area_m2 / 1e4
f$area_km2 <- f$area_m2 / 1e6
f

plot(m_class)


huc_id <- "041402011103"

# --- HUC12 polygon: pass SQL via `query=` (2nd positional arg is `layer`, not a query) ---
huc_sf <- st_read(
  "../NYS_Wetlands_Data/Data/NY_HUCS/NY_Cluster_Zones_250_CROP_NAomit_6347.gpkg",
  query = paste0(
    "SELECT * FROM NY_Cluster_Zones_250_CROP_NAomit_6347 WHERE huc12 = '",
    huc_id,
    "'"
  ),
  quiet = TRUE
)

# --- NWI polygons: read the NY_Wetlands layer (layer 1 is a state boundary!),
#     only features intersecting the HUC (avoids loading all 915k NY polygons) ---
nwi_sf <- st_read(
  "../NYS_Wetlands_Data/Data/NWI/NY_NWI_6347.gpkg",
  layer = "NY_Wetlands",
  wkt_filter = st_as_text(st_geometry(huc_sf)),
  quiet = TRUE
)

# drop open-water + riverine; keep vegetated wetland classes (matches model EMW/FSW/SSW)
open_water_types <- c(
  "Freshwater Pond",
  "Lake",
  "Riverine",
  "Estuarine and Marine Deepwater"
)
nwi_sf <- nwi_sf[!nwi_sf$WETLAND_TYPE %in% open_water_types, ]

# clip to the HUC boundary, then sum area (both layers are EPSG:6347, meters)
nwi_clip <- st_intersection(nwi_sf, huc_sf)
nwi_clip$area_m2 <- as.numeric(st_area(nwi_clip))

# per-type breakdown
nwi_by_type <- aggregate(area_m2 ~ WETLAND_TYPE, data = nwi_clip, FUN = sum)
nwi_by_type$area_ha <- nwi_by_type$area_m2 / 1e4
print(nwi_by_type[order(-nwi_by_type$area_ha), c("WETLAND_TYPE", "area_ha")])

nwi_total_ha <- sum(nwi_clip$area_m2) / 1e4
nwi_total_km2 <- sum(nwi_clip$area_m2) / 1e6
cat(sprintf(
  "NWI vegetated wetland area in HUC %s: %.1f ha (%.2f km2)\n",
  huc_id,
  nwi_total_ha,
  nwi_total_km2
))
