library(terra)

#d <- rast("Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/All_WA/data/Rasters/HLEF_DTM_EPSG_6394.tif")
d <- list.files("Data/DEMs/imgs", full.names = TRUE)[20:27] |> 
    lapply(rast) |> terra::sprc() |> terra::merge()
slpasp <- terrain(d, v = c("slope", "aspect"), unit = "radians")
hs <- terra::shade(slope = slpasp$slope, aspect = slpasp$aspect)

agg_w <- d |> terra::focal(w = 5, fun = "mean", na.rm = TRUE) 
agg <- d |> terra::aggregate(5, fun = "mean", na.rm = TRUE) 
agg_smooth_cube <- terra::resample(agg, y = terra::rast(d), method = "cubicspline")
agg_smooth_lanz <- terra::resample(agg, y = terra::rast(d), method = "lanczos")
#plot(agg)
plot(c(d, agg_w, agg_smooth_cube, agg_smooth_lanz), col=grey(0:100/100), nc = 2, main = c("dem", "agg_w", "cube", "lanz"))


metric_norm <-  d |> # aggregate first
    MultiscaleDTM::Qfit(metrics = "profc") |> terra::focal(w = 5, fun = "mean", na.rm = TRUE)
metric_norm_agg <-  agg_w |> # aggregate first
    MultiscaleDTM::Qfit(metrics = "profc")
metric_cube <-  agg_smooth_cube |> # aggregate first
    MultiscaleDTM::Qfit(metrics = "profc")
metric_lanz <-  agg_smooth_lanz |> # aggregate first
    MultiscaleDTM::Qfit(metrics = "profc")
plot(c(metric_norm, metric_norm_agg, metric_cube, metric_lanz), col=grey(0:100/100), nc = 2, main = c("hs", "norm", "cube", "lanz"))

range(values(metric_norm, na.rm = TRUE))
range(values(metric_cube, na.rm = TRUE))
range(values(metric_lanz, na.rm = TRUE))

sd(values(metric_norm, na.rm = TRUE))/abs(mean(values(metric_norm, na.rm = TRUE)))
sd(values(metric_cube, na.rm = TRUE))/abs(mean(values(metric_cube, na.rm = TRUE)))
sd(values(metric_lanz, na.rm = TRUE))/abs(mean(values(metric_lanz, na.rm = TRUE)))




