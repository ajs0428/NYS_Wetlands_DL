### Reclassifies all current R_Patches Rasters to remove open water wetland ()

library(terra)
library(sf) 
library(dplyr)
library(tidyr)
library(stringr)
library(tidyterra)
library(readr)

set.seed(11)

########################################################################################

l_patches <- list.files("Data/Training_Data/R_Patches/", pattern = ".tif$", full.names = TRUE) 

m <- c(0, 0,
       1, 1,
       2, 3,
       3, 2, 
       4, 3)
rclmat <- matrix(m, ncol = 2, byrow = TRUE)

remove_oww <- function(patch_file){
    fn <- paste0("Data/Training_Data/R_Patches_NOW/", basename(patch_file))
    p <- rast(patch_file)
    p[["MOD_CLASS"]] <- classify(p$MOD_CLASS, rclmat, include.lowest = TRUE)
    
    writeRaster(p, fn, overwrite = TRUE)
    
    return(NULL)
}

lapply(l_patches, remove_oww)
