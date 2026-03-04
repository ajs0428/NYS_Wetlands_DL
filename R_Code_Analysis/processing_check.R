library(terra)

l <- list.files("Data/TerrainProcessed/HUC_TerrainMetrics/", "*.tif", full.names = TRUE)
lfs <- lapply(l, file.size) |> lapply(function(x) x/1E9)

hist(unlist(lfs), 50)

df <- data.frame(filename = unlist(l), size = unlist(lfs))
df_f <- df |> filter(size < 0.1) 

results <- list()  # Initialize an empty list to store results

for (i in df_f$filename){
    had_error <- FALSE  # Flag to track if an error occurred
    
    result <- tryCatch({
        rast(i)
    }, error = function(e) {
        # message("An error occurred: ", e$message)
        had_error <<- TRUE  # Use <<- to modify the outer variable
        return(NULL)
    }, warning = function(w) {
        # message("A warning occurred: ", w$message)
        had_error <<- TRUE
        return(NULL)
    }, finally = {
        # message("NoError")
    })
    
    # If there was an error, store the filename; otherwise store the result
    if (had_error) {
        results[[length(results) + 1]] <- i
    } else {
        results[[length(results) + 1]] <- "NoError"
    }
}

bad_results <- unlist(results)[unlist(results) != "NoError"]

writeLines(bad_results, "Data/ErrorTracking/HUC_TerrainProcessed_bad_results.txt")
