library(terra)

spec <- terra::vrt(list.files("Data/Satellite/",
                              pattern = ".img$|.tif",
                              full.names = TRUE))
v <- vect("Data/NY_HUCS/NY_HUCS_08_6350_Cluster.gpkg") |> 
  tidyterra::filter(CLUSTER_ID == 11) |> 
  terra::project(crs(spec))


crp <- crop(spec, v)
plot(crp)
names(crp) <- names(rast("Data/Satellite/GEE_Asset0000000000-0000000000.tiff"))
crp
spec[1]
names(rast("Data/Satellite/GEE_Asset0000000000-0000000000.tiff"))

writeRaster(crp, "Data/Satellite/NYS_Clust11_spec.tif")
