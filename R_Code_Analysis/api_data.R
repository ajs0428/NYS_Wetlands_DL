args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg",
    208,
    "Data/CHMs/AWS"
)


clt <- sf::st_read(args[1], quiet = TRUE)

chm_hucs <- list.files("Data/CHMs/HUC_CHMs/", "*.tif") |> 
    str_extract("(?<=huc_)\\d+")
ter500_hucs <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", "*500m.tif") |> 
    str_extract("(?<=huc_)\\d+") |> 
    unique()
ter100_hucs <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", "*500m.tif") |> 
    str_extract("(?<=huc_)\\d+") |> 
    unique()
ter5_hucs <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", "*500m.tif") |> 
    str_extract("(?<=huc_)\\d+") |> 
    unique()
hydro_hucs <- list.files("Data/TerrainProcessed/HUC_Hydro/", pattern = "wbt_TWI_Facc") |> 
    str_extract("(?<=huc_)\\d+") |> 
    unique()
naip_hucs <- list.files("Data/NAIP/HUC_NAIP_Processed/", pattern = ".tif") |> 
    str_extract("(?<=huc_)\\d+") |> 
    unique()

clt$huc12[!clt$huc12 %in% chm_hucs]

length(chm_hucs)
length(ter500_hucs)
length(ter100_hucs)
length(ter5_hucs)
length(hydro_hucs)
length(naip_hucs)



library(httr)
library(sf)
library(jsonlite)

# Query all features (may need pagination for large datasets)
url <- "https://elevation.its.ny.gov/arcgis/rest/services/Dem_Indexes/FeatureServer/2/query"
response <- GET(url, query = list(
    where = "1=1", 
    outFields = "*",
    f = "geojson"
))

# Parse and convert to sf object
data <- st_read(content(response, "text"), quiet = TRUE)
data


library(arcgislayers)

url <- "https://elevation.its.ny.gov/arcgis/rest/services/Dem_Indexes/FeatureServer/2"
layer <- arc_open(url)

# This automatically handles pagination
data <- arc_select(layer)

cat("Total features downloaded:", nrow(data), "\n")

st_write(data, "Data/NYS_DEM_Indexes/FEMA_Bare_Earth_DEM_1m.shp")
