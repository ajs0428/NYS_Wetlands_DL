#!/usr/bin/env Rscript

args = c("Data/NAIP/",
         208,
         "Data/TerrainProcessed/HUC_DEMs/",
         "Data/TerrainProcessed/HUC_Hydro/"
)
# args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to a file vector study area", args[1], "\n",
    "- Cluster number (integer 1-200ish):", args[2], "\n",
    "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
    "- Path to save folder:", args[4], "\n"
)

###############################################################################################

library(terra)
library(sf)
library(whitebox)
library(MultiscaleDTM)
library(foreach)
library(doParallel)
suppressPackageStartupMessages(library(tidyterra))

terraOptions(memfrac = 0.10,# Use only 10% of memory for program
             memmax = 8, #max memory is 8Gb
             tempdir = "/ibstorage/anthony/NYS_Wetlands_GHG/Data/tmp")
###############################################################################################

n <- terra::rast("/ibstorage/NAIP/NY-selected/ny_n/ortho_1-1_hn_ny053_2021_1.shp")