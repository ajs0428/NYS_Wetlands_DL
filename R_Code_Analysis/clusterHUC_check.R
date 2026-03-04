library(terra)
library(sf)

cls <- st_read("Data/NY_HUCS/NY_Cluster_Zones_200.gpkg")

cls_nums <- c(11, 12, 22, 51, 53, 56, 60, 64, 67, 84, 86, 90, 
               92, 102, 105, 116, 120, 123, 136, 138, 152, 176,
               183, 189, 192, 193, 198, 208, 218, 225, 250)

df <- cls |> filter(cluster %in% cls_nums) |> 
    as.data.frame() |> 
    select(huc12) 

writeLines(df$huc12, "Data/Dataframes/HUCs_in_site_clusters.txt")

cls_filter <- cls |> filter(cluster %in% cls_nums)

cls_filter[cls_filter$huc12 == "020200030406", ] |> na.omit() |> st_geometry() |> vect() |> plet()

cls |> na.omit() |> st_write("Data/NY_HUCS/NY_Cluster_Zones_250_NAomit.gpkg")

dfna <- cls |> na.omit() |> filter(cluster %in% cls_nums) |> 
    as.data.frame() |> 
    select(huc12) 
writeLines(dfna$huc12, "Data/Dataframes/HUCs_in_site_clusters_NAomit.txt")
