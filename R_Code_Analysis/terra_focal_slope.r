library(terra)
library(MultiscaleDTM)
library(rgeomorphon)

r <- rast("Data/DEMs/FEMA_2019_DEM/Broome/18TVM080920.tif")
plot(r)

r_agg <- terra::aggregate(r, fact = 50, fun = "median")
r_agg_rsp <- resample(r_agg, r, method = "cubicspline")

slp <- terra::terrain(r, v = "slope")
slp2 <- MultiscaleDTM::SlpAsp(r,metrics = c("slope"),
                              w = c(3,3), na.rm = TRUE)
slp3 <- terra::terrain(r_agg_rsp, v = "slope")

slp_f <- terra::focal(slp, w = 51, na.rm = TRUE, fun = "median")
slp_w <- MultiscaleDTM::SlpAsp(r,metrics = c("slope"),
                              w = c(51,51), na.rm = TRUE)
slp_a <- terra::focal(slp3, w = 51, na.rm = TRUE, fun = "median")

slp_pts <- spatSample(x = slp_f, size = 100, as.points = TRUE) |> 
    terra::extract(x = slp_w, bind = T, exact = TRUE) |> 
    terra::extract(x = slp_a, bind = T, exact = TRUE) 
names(slp_pts) <- c("slope1", "slope2", "slope3")
plot(slp_pts$slope1, slp_pts$slope2)
abline(0, 1)
plot(slp_pts$slope1, slp_pts$slope3)
abline(0, 1)

plot(c(slp, slp_f, slp_w, slp_a), main = c("slp regular", 
                                           "slp median filter",
                                           "slp Multiscale Window",
                                           "slp agg then filter"), 
     range = c(0, 45))


rk <- terra::k_means(c(r, slp), 5, maxcell = 1E3)
plot(rk)


############### TPI & TRI
ti <- terra::terrain(r, v = c("slope", "TPI", "flowdir", "aspect"), unit = "radians")
plot(ti)
ti_pts <- spatSample(x = ti, size = 100, as.points = TRUE)
cor(ti_pts$slope, ti_pts$TRI)

############### Geomorphons

gm <- rgeomorphon::geomorphons(elevation = r, 
                               search = 100, 
                               use_meters = TRUE,
                               skip = 5, 
                               flat_angle_deg = 1.5)
hs <- terra::shade(slope = ti$slope, aspect = ti$aspect)
plot(r)
plot(hs, add = T, col=grey(0:100/100), alpha = 0.5)
plot(c(gm))
plot(hs, add = T, col=grey(0:100/100), alpha = 0.5)

gm_smooth <- terra::focal(gm, w = 11, fun = "modal")
plot(c(gm, gm_smooth))

