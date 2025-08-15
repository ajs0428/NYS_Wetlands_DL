
out_dir <- "WIP/"
dem_dir <- "WIP/dems_test/"
len = 100
exec_dir <- "WIPbook-master/ExecutableFiles/"

file_name <- paste0("WIP/", "input_makeGrids.txt")
file.create(file_name)

writeLines(c("# Input file for makeGrids",
             "",
             paste0("DEM: ", dem_dir),
             paste0("SCRATCH DIRECTORY: ", out_dir),
             paste0("LENGTH SCALE: ", len)), con = file_name)

write(paste0("GRID: GRADIENT, OUTPUT FILE = ", out_dir, "grad", len, ".flt"),
      file = file_name, append = T)

# Run surface metrics sans DEV
system(paste0("wine ", "/ibstorage/anthony/NYS_Wetlands_GHG/WIPbook-master/ExecutableFiles/MakeGrids.exe"), input = file_name)
