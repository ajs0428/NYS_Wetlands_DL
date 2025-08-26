#!/usr/bin/env Rscript

args = c(
         "Data/NAIP/",
         "Data/NAIP/NAIP_Processed/"
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

(cat("these are the arguments: \n", 
    "- Path to a file unprocessed NAIP files:", args[1], "\n",
    "- Path to processed NAIP files:", args[2], "\n"
))

###############################################################################################

library(terra)
library(sf)
suppressPackageStartupMessages(library(tidyverse))
library(foreach)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 64, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################


hydro_list <- list.files("Data/TerrainProcessed/HUC_Hydro/", pattern = ".tif", full.names = TRUE, recursive = FALSE)

naip_unproc_list <- list.files(args[1], pattern = ".tif", full.names = TRUE, recursive = FALSE)
###############################################################################################

naip_processing_func <- function(naip_list, ref_rast_list){
    vi2 <- function(r, g, nir) {
        return(
            c(((nir - r) / (nir + r)), ((g-nir)/(g+nir)))
            )
    }
    target_crs <- terra::crs(rast(hydro_list[[1]]))
    target_folder_files <- list.files(args[2], recursive = FALSE)
    # naip_processed <- list()
    for(i in seq_along(naip_list)){
        if(!file.exists(paste0(args[2], "Metrics_", basename(naip_list[[i]])))){
            print("no NAIP processed yet")
            n <- rast(naip_list[[i]]) |>
                terra::project(target_crs, res = 1)
            #np <- lapp(c(n[[1]], n[[2]], n[[4]]), fun = vi2, cores = 4)
            np <- vi2(n[[1]], n[[2]], n[[4]])
            print(names(np))
            nall <- c(n, np)
            set.names(nall, c("r", "g", "b", "nir", "ndvi", "ndwi"))
            writeRaster(nall, paste0(args[2], "Metrics_", basename(naip_list[[i]])),
                        overwrite = TRUE)
        } else {
            print("NAIP already processed")
        }
        
    }
    #return(naip_processed)
}

system.time({naip_processing_func(naip_unproc_list, hydro_list)})


# nl <- naip_unproc_list[str_detect(naip_unproc_list, "4207619_se_18_060_20210824")]
# r <- rast(nl)
# system.time({rp <- vi2(r[[1]], r[[2]], r[[4]])})
# system.time({rpl <- lapp(c(r[[1]], r[[2]], r[[4]]), fun = vi2, cores = 2)})
# plot(c(rp,rpl))
