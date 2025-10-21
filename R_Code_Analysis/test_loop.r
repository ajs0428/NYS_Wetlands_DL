#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)

# (cat("these are the arguments: \n", 
#     "- Path to a file vector study area", args[1], "\n",
#     "- Cluster number (integer 1-200ish):", args[2], "\n",
#     "- Path to the DEMs in TerrainProcessed folder", args[3], "\n",
#     "- Path to save folder:", args[4], "\n"))

file <- paste0("Shell_Scripts/testing/", args[2], ".txt")
writeLines(args[2], con = file)
print("#############################################################")