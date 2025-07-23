library(lidR)
library(rgl)
library(lidRviewer)


pc <- readLAS("/Users/Anthony/Data and Analysis Local/NYS_Wetlands_GHG/Data/LIDAR/3101_046615.las",
              filter = "-keep_first")
pcl <- lidR::decimate_points(pc, algorithm = random(0.5))
pcln <- normalize_height(pcl, knnidw())
pclf <- lidR::filter_poi(pcln, Z >= 0 & Z < quantile(Z, .98))
pclf
lidR::plot(pclf, color = "Z")
rglwidget()

view(pclf)

    