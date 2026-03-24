library(RCurl)
library(curl)

url_link <- "ftp://ftp.gis.ny.gov/elevation/LIDAR/"
url_link <- "ftp://ftp.gis.ny.gov/ortho/nysdop10/"

con <- curl(url_link, open = "r")
lidar_l <- readLines(con)
lidar_l_names <- lidar_l |> 
    str_split("\\s+") |> 
    map_chr(~ .x[length(.x)])

chm_l <- list.files("Data/CHMs/AWS/", pattern = ".gpkg") |> str_remove("chm_") |> str_remove(".gpkg")
dem_l <- list.dirs("Data/DEMs/",recursive = F, full.names = F) 

dem_l[dem_l %in% lidar_l_names]
chm_l[!chm_l %in% lidar_l_names]
