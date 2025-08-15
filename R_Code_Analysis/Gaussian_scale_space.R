# Gaussian Scale-Space Implementation for Real DEM Data
# Using terra package for efficient raster processing

library(terra)
library(viridis)
library(RColorBrewer)

# ============================================================================
# PART 1: Core Gaussian Scale-Space Functions (Optimized for Terra)
# ============================================================================

# Fast box filter using terra's focal function for edge handling
box_filter_terra <- function(raster_data, radius) {
    # Create a square weight matrix for the box filter
    w <- matrix(1, nrow = 2*radius + 1, ncol = 2*radius + 1)
    w <- w / sum(w)  # Normalize to get average
    
    # Apply focal filter (terra handles edges properly)
    filtered <- focal(raster_data, w = w, fun = "sum", na.rm = TRUE)
    
    return(filtered)
}

# Optimized Gaussian scale-space for terra rasters
gaussian_scale_space_terra <- function(dem_raster, sigma, n_iterations = 5) {
    # This implements the fast Gaussian approximation for terra SpatRaster objects
    
    # Get the resolution to scale sigma appropriately
    res_x <- res(dem_raster)[1]
    
    # Adjust sigma to account for pixel size (convert from ground units to pixels)
    sigma_pixels <- sigma / res_x
    
    # Calculate ideal box filter width
    w_ideal <- sqrt(12 * sigma_pixels^2 / n_iterations + 1)
    
    # Get odd integer widths
    w_low <- floor(w_ideal)
    if(w_low %% 2 == 0) w_low <- w_low - 1
    w_high <- w_low + 2
    
    # Convert to radius
    radius_low <- (w_low - 1) / 2
    radius_high <- (w_high - 1) / 2
    
    # Calculate number of iterations for each filter size
    m <- round((12 * sigma_pixels^2 - n_iterations * w_low^2 - 
                    4 * n_iterations * w_low - 3 * n_iterations) / 
                   (-4 * w_low - 4))
    m <- max(0, min(n_iterations, m))
    
    # Apply filters iteratively
    result <- dem_raster
    
    # Apply low-radius filter m times
    if(m > 0 && radius_low > 0) {
        for(i in 1:m) {
            result <- box_filter_terra(result, ceiling(radius_low))
        }
    }
    
    # Apply high-radius filter (n_iterations - m) times
    if(m < n_iterations && radius_high > 0) {
        for(i in 1:(n_iterations - m)) {
            result <- box_filter_terra(result, ceiling(radius_high))
        }
    }
    
    # Preserve original CRS and names
    crs(result) <- crs(dem_raster)
    names(result) <- paste0(names(dem_raster), "_sigma", round(sigma))
    
    return(result)
}

# ============================================================================
# PART 2: DEM Loading and Preprocessing Functions
# ============================================================================

load_and_prepare_dem <- function(dem_path) {
    # Load DEM from file at full resolution
    
    cat(paste("Loading DEM from:", dem_path, "\n"))
    
    # Load the DEM
    dem <- rast(dem_path)
    
    # Check if it's elevation data
    if(nlyr(dem) > 1) {
        warning("Multiple layers detected. Using first layer as elevation.")
        dem <- dem[[1]]
    }
    
    # Ensure we have a proper CRS
    if(is.na(crs(dem))) {
        warning("No CRS detected. Assuming projected coordinate system.")
    }
    
    # Basic DEM statistics
    cat("\nDEM Properties:\n")
    cat(sprintf("  Dimensions: %d rows × %d columns\n", nrow(dem), ncol(dem)))
    cat(sprintf("  Cell count: %d\n", ncell(dem)))
    cat(sprintf("  Resolution: %.2f × %.2f %s\n", 
                res(dem)[1], res(dem)[2],
                ifelse(is.lonlat(dem), "degrees", "units")))
    cat(sprintf("  Extent: X[%.2f, %.2f], Y[%.2f, %.2f]\n",
                xmin(dem), xmax(dem), ymin(dem), ymax(dem)))
    
    # Handle NA values
    na_count <- sum(is.na(values(dem)))
    if(na_count > 0) {
        cat(sprintf("  NA values: %d (%.1f%%)\n", 
                    na_count, 100 * na_count / ncell(dem)))
    }
    
    # Elevation statistics - Fixed the error here
    elev_values <- values(dem, na.rm = TRUE)
    if(length(elev_values) > 0) {
        cat(sprintf("  Elevation range: %.1f to %.1f\n",
                    min(elev_values), max(elev_values)))
        cat(sprintf("  Mean elevation: %.1f\n", mean(elev_values)))
        cat(sprintf("  Std deviation: %.1f\n", sd(elev_values)))
    }
    
    return(dem)
}

# ============================================================================
# PART 3: Terrain Analysis Functions
# ============================================================================

calculate_terrain_metrics <- function(dem, metric = "slope") {
    # Calculate various terrain metrics using terra's built-in functions
    
    result <- switch(metric,
                     "slope" = terrain(dem, v = "slope", unit = "degrees"),
                     "aspect" = terrain(dem, v = "aspect", unit = "degrees"),
                     "TPI" = terrain(dem, v = "TPI"),  # Topographic Position Index
                     "TRI" = terrain(dem, v = "TRI"),  # Terrain Ruggedness Index
                     "roughness" = terrain(dem, v = "roughness"),
                     "flowdir" = terrain(dem, v = "flowdir"),
                     stop("Unknown metric. Choose: slope, aspect, TPI, TRI, roughness, flowdir")
    )
    
    return(result)
}

# ============================================================================
# PART 4: Multiscale Analysis Pipeline
# ============================================================================

multiscale_terrain_analysis <- function(dem, 
                                        sigmas = c(0, 5, 10, 25, 50, 100),
                                        metrics = c("slope", "TRI"),
                                        plot_results = TRUE) {
    
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("MULTISCALE TERRAIN ANALYSIS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    # Store results
    results <- list()
    results$dem <- list()
    results$metrics <- list()
    results$statistics <- list()
    
    # Get DEM resolution for scale interpretation
    res_m <- res(dem)[1]
    if(is.lonlat(dem)) {
        # Convert degrees to meters (approximate at center latitude)
        center_lat <- (ymax(dem) + ymin(dem)) / 2
        res_m <- res_m * 111320 * cos(center_lat * pi / 180)
    }
    
    # Process each scale
    for(i in 1:length(sigmas)) {
        sigma <- sigmas[i]
        
        cat(sprintf("Processing scale %d/%d (σ = %.0f meters)...\n", 
                    i, length(sigmas), sigma))
        
        if(sigma == 0) {
            # Original DEM
            smoothed_dem <- dem
            scale_label <- "Original"
        } else {
            # Apply Gaussian smoothing
            smoothed_dem <- gaussian_scale_space_terra(dem, sigma)
            scale_label <- sprintf("σ=%.0fm", sigma)
            
            # Approximate wavelength cutoff
            wavelength <- 2 * pi * sigma / sqrt(2 * log(2))
            cat(sprintf("  Approximate wavelength cutoff: %.1f meters\n", wavelength))
            cat(sprintf("  Features < %.1f meters are suppressed\n", sigma * 3))
        }
        
        # Store smoothed DEM
        results$dem[[scale_label]] <- smoothed_dem
        
        # Calculate metrics for this scale
        results$metrics[[scale_label]] <- list()
        for(metric in metrics) {
            cat(sprintf("  Calculating %s...\n", metric))
            results$metrics[[scale_label]][[metric]] <- 
                calculate_terrain_metrics(smoothed_dem, metric)
        }
        
        # Collect statistics
        results$statistics[[scale_label]] <- list(
            sigma = sigma,
            elev_sd = global(smoothed_dem, "sd", na.rm = TRUE)[[1]],
            elev_range = diff(range(values(smoothed_dem), na.rm = TRUE))
        )
        
        if("slope" %in% metrics) {
            slope_stats <- global(results$metrics[[scale_label]][["slope"]], 
                                  c("mean", "sd", "max"), na.rm = TRUE)
            results$statistics[[scale_label]]$slope_mean <- slope_stats[1, "mean"]
            results$statistics[[scale_label]]$slope_sd <- slope_stats[1, "sd"]
            results$statistics[[scale_label]]$slope_max <- slope_stats[1, "max"]
        }
    }
    
    cat("\nAnalysis complete!\n\n")
    
    # Print summary statistics
    print_scale_statistics(results$statistics)
    
    # Create visualizations if requested
    if(plot_results) {
        visualize_multiscale_results(results, sigmas, metrics)
    }
    
    return(results)
}

# ============================================================================
# PART 5: Visualization Functions
# ============================================================================

visualize_multiscale_results <- function(results, sigmas, metrics) {
    # Create comprehensive visualization of multiscale analysis
    
    n_scales <- length(sigmas)
    n_metrics <- length(metrics) + 1  # +1 for elevation
    
    # Set up plot layout
    par(mfrow = c(n_metrics, min(n_scales, 6)), 
        mar = c(2, 2, 3, 4),
        oma = c(0, 0, 2, 0))
    
    # Color palettes
    elev_colors <- terrain.colors(100)
    slope_colors <- colorRampPalette(c("green", "yellow", "orange", "red", "darkred"))(100)
    tri_colors <- colorRampPalette(c("darkblue", "blue", "cyan", "yellow", "red"))(100)
    
    # Plot elevation at each scale
    for(i in 1:min(n_scales, 6)) {
        scale_label <- names(results$dem)[i]
        dem_data <- results$dem[[scale_label]]
        
        plot(dem_data, main = paste("Elevation -", scale_label),
             col = elev_colors, axes = FALSE,
             legend = (i == min(n_scales, 6)))
    }
    
    # Plot each metric at each scale
    for(metric in metrics) {
        metric_colors <- switch(metric,
                                "slope" = slope_colors,
                                "TRI" = tri_colors,
                                viridis(100))
        
        for(i in 1:min(n_scales, 6)) {
            scale_label <- names(results$metrics)[i]
            metric_data <- results$metrics[[scale_label]][[metric]]
            
            plot(metric_data, 
                 main = paste(toupper(metric), "-", scale_label),
                 col = metric_colors, axes = FALSE,
                 legend = (i == min(n_scales, 6)))
        }
    }
    
    # Add overall title
    mtext("Multiscale Terrain Analysis", outer = TRUE, cex = 1.5, font = 2)
}

print_scale_statistics <- function(stats) {
    # Print formatted statistics table
    
    cat("SCALE-DEPENDENT STATISTICS:\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    
    # Extract scale labels and values
    scales <- names(stats)
    
    # Print header
    cat(sprintf("%-15s %10s %12s %12s %12s\n", 
                "Scale", "Elev SD", "Elev Range", "Mean Slope", "Max Slope"))
    cat(paste(rep("-", 70), collapse = ""), "\n")
    
    # Print statistics for each scale
    for(scale in scales) {
        s <- stats[[scale]]
        
        # Format values, handling missing metrics gracefully
        slope_mean <- ifelse(is.null(s$slope_mean), "N/A", 
                             sprintf("%.1f°", s$slope_mean))
        slope_max <- ifelse(is.null(s$slope_max), "N/A", 
                            sprintf("%.1f°", s$slope_max))
        
        cat(sprintf("%-15s %10.1f %12.1f %12s %12s\n",
                    scale,
                    s$elev_sd,
                    s$elev_range,
                    slope_mean,
                    slope_max))
    }
    cat(paste(rep("-", 70), collapse = ""), "\n")
}

# ============================================================================
# PART 6: Scale Selection Functions
# ============================================================================

determine_optimal_scales <- function(dem, min_feature_size = NULL, 
                                     max_feature_size = NULL,
                                     n_scales = 6) {
    # Automatically determine appropriate scales based on DEM characteristics
    
    # Get resolution
    res_m <- res(dem)[1]
    if(is.lonlat(dem)) {
        center_lat <- (ymax(dem) + ymin(dem)) / 2
        res_m <- res_m * 111320 * cos(center_lat * pi / 180)
    }
    
    # Determine extent
    extent_x <- (xmax(dem) - xmin(dem))
    extent_y <- (ymax(dem) - ymin(dem))
    if(is.lonlat(dem)) {
        center_lat <- (ymax(dem) + ymin(dem)) / 2
        extent_x <- extent_x * 111320 * cos(center_lat * pi / 180)
        extent_y <- extent_y * 111320
    }
    max_extent <- min(extent_x, extent_y)
    
    # Set scale range
    if(is.null(min_feature_size)) {
        min_feature_size <- res_m * 3  # Minimum meaningful scale
    }
    if(is.null(max_feature_size)) {
        max_feature_size <- max_extent / 10  # 1/10 of extent
    }
    
    # Generate logarithmically spaced scales
    # Convert to sigma (approximate: feature_size ≈ 3*sigma)
    min_sigma <- min_feature_size / 3
    max_sigma <- max_feature_size / 3
    
    # Include 0 (original) and generate other scales
    sigmas <- c(0, exp(seq(log(min_sigma), log(max_sigma), 
                           length.out = n_scales - 1)))
    
    cat("\nRecommended scale parameters (sigma in meters):\n")
    for(i in 1:length(sigmas)) {
        if(sigmas[i] == 0) {
            cat(sprintf("  Scale %d: Original (no smoothing)\n", i))
        } else {
            cat(sprintf("  Scale %d: σ = %.1f m (captures features > %.1f m)\n", 
                        i, sigmas[i], sigmas[i] * 3))
        }
    }
    
    return(sigmas)
}

# ============================================================================
# PART 7: Export Functions
# ============================================================================

export_multiscale_results <- function(results, output_dir = "multiscale_output",
                                      format = "GTiff") {
    # Export results to files for use in GIS software
    
    # Create output directory
    if(!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        cat(sprintf("Created output directory: %s\n", output_dir))
    }
    
    # Export smoothed DEMs
    for(scale_label in names(results$dem)) {
        filename <- file.path(output_dir, 
                              sprintf("dem_%s.tif", 
                                      gsub("[^A-Za-z0-9]", "_", scale_label)))
        writeRaster(results$dem[[scale_label]], filename, 
                    filetype = format, overwrite = TRUE)
        cat(sprintf("Exported: %s\n", filename))
    }
    
    # Export metrics
    for(scale_label in names(results$metrics)) {
        for(metric in names(results$metrics[[scale_label]])) {
            filename <- file.path(output_dir, 
                                  sprintf("%s_%s.tif", metric,
                                          gsub("[^A-Za-z0-9]", "_", scale_label)))
            writeRaster(results$metrics[[scale_label]][[metric]], 
                        filename, filetype = format, overwrite = TRUE)
            cat(sprintf("Exported: %s\n", filename))
        }
    }
    
    # Export statistics as CSV
    stats_df <- do.call(rbind, lapply(names(results$statistics), function(scale) {
        data.frame(scale = scale, results$statistics[[scale]], 
                   stringsAsFactors = FALSE)
    }))
    
    csv_file <- file.path(output_dir, "scale_statistics.csv")
    write.csv(stats_df, csv_file, row.names = FALSE)
    cat(sprintf("Exported statistics: %s\n", csv_file))
}

# ============================================================================
# PART 8: Main Analysis Function
# ============================================================================

run_gaussian_scale_analysis <- function(dem_path, 
                                        sigmas = NULL,
                                        metrics = c("slope", "TRI"),
                                        output_dir = NULL,
                                        export_results = FALSE,
                                        plot_results = TRUE) {
    
    cat("\n=== GAUSSIAN SCALE-SPACE DEM ANALYSIS ===\n")
    cat(paste(rep("=", 40), collapse = ""), "\n\n")
    
    # Load DEM from file
    dem <- load_and_prepare_dem(dem_path)
    
    # Determine scales if not provided
    if(is.null(sigmas)) {
        sigmas <- determine_optimal_scales(dem, n_scales = 6)
    } else {
        cat("\nUsing provided scale parameters:\n")
        for(i in 1:length(sigmas)) {
            if(sigmas[i] == 0) {
                cat(sprintf("  Scale %d: Original (no smoothing)\n", i))
            } else {
                cat(sprintf("  Scale %d: σ = %.1f m\n", i, sigmas[i]))
            }
        }
    }
    
    # Run multiscale analysis
    results <- multiscale_terrain_analysis(
        dem = dem,
        sigmas = sigmas,
        metrics = metrics,
        plot_results = plot_results
    )
    
    # Export if requested
    if(export_results) {
        if(is.null(output_dir)) {
            output_dir <- paste0("multiscale_", 
                                 format(Sys.Date(), "%Y%m%d"))
        }
        export_multiscale_results(results, output_dir)
    }
    
    return(results)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("GAUSSIAN SCALE-SPACE FOR DEM ANALYSIS\n")
cat("======================================\n\n")
results <- run_gaussian_scale_analysis(
    dem_path = "Data/TerrainProcessed/cluster_208_huc_041402011103.tif",
    sigmas = c(0, 5, 10, 25, 50, 100, 250),
    metrics = c("slope"),
    export_results = FALSE,
    output_dir = "Data/TerrainProcessed/"
)

cat("Or use automatic scale selection:\n")
cat("----------------------------------\n")
cat("results <- run_gaussian_scale_analysis(\n")
cat('  dem_path = "path/to/your/dem.tif",\n')
cat("  sigmas = NULL,  # Will auto-determine\n")
cat('  metrics = c("slope", "TRI")\n')
cat(")\n\n")

cat("The results object contains:\n")
cat("  - results$dem: Smoothed DEMs at each scale\n")
cat("  - results$metrics: Terrain metrics at each scale\n")
cat("  - results$statistics: Summary statistics\n\n")

# Quick function to analyze a single DEM at one scale
quick_smooth <- function(dem_path, sigma) {
    dem <- load_and_prepare_dem(dem_path)
    smoothed <- gaussian_scale_space_terra(dem, sigma)
    cat(sprintf("\nSmoothed DEM with σ = %.1f meters\n", sigma))
    return(smoothed)
}