#!/usr/bin/env Rscript

args = c(
    "Data/NY_HUCS/NY_Cluster_Zones_200.gpkg",
    22,
    "Data/CHMs/AWS",
    "Data/CHMs/AWS"
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
suppressPackageStartupMessages(library(tidyterra))
library(future)
library(future.apply)
###############################################################################################

cluster_target <- sf::st_read(args[1], quiet = TRUE) |> 
    sf::st_transform(st_crs("EPSG:6347")) |>
    dplyr::filter(cluster == args[2]) |> 
    terra::vect() 

###############################################################################################

# This is all the DEM file names 
chms_file_list <- list.files(args[4], 
                             pattern = ".gpkg$", 
                             full.names = TRUE, 
                             recursive = FALSE, 
                             include.dirs = FALSE)
print(paste0("this is the total list of chm indexes: ", length(chms_file_list)[[1]]))
###############################################################################################

cluster_extract <- function(cluster, chm_indexes){
    files_to_extract <- list()
    for(i in seq_along(chm_indexes)){
        chms_in <- chm_indexes[[i]] |> st_filter(y = cluster, .predicate = st_intersects)
        if(nrow(chms_in) > 1){
            Fnames <- chms_in["FILENAME"][[1]]
            files_to_extract <- append(files_to_extract, Fnames)
        } else {
            next
        }
    }
    return(files_to_extract)
}

###############################################################################################
list.dirs(args[1], recursive = FALSE, full.names = TRUE)

function(chm_dir){
    vr <- vrt(chm_dir) 
}