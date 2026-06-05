### huc_stack.R
###
### Single source of truth for assembling the per-HUC predictor stack used by
### the deep-learning pipeline. Sourced by:
###   - Raster_ChipsPatches_DL.R          (crop training chips, per patch)
###   - Point_Extraction_DL.R             (extract predictor values at points)
###   - DL_Extract_Normalize_Stats_FullRasters.R  (global min/max per band)
###
### Nothing is ever written to disk here. Stacks are assembled lazily in memory
### so source rasters (CHM, DEM, terrain, hydro, NAIP, lidar) are read directly
### from their canonical locations and never duplicated into a saved *_stack.tif.
###
### The band recipe (which layers, in what order, with which transforms) lives
### ONLY in this file. That is the contract handed to the downstream PyTorch
### training/inference code: train chips, stats bands, and point extractions are
### all guaranteed identical because they come from the same builder.

suppressPackageStartupMessages({
  library(terra)
  library(stringr)
  library(tidyterra)
})

## --- Canonical source directories (relative to project root) -----------------
huc_source_dirs <- function() {
  list(
    dem   = "Data/TerrainProcessed/HUC_DEMs/",
    terr  = "Data/TerrainProcessed/HUC_TerrainMetrics/",
    hydro = "Data/TerrainProcessed/HUC_Hydro/",
    chm   = "Data/CHMs/HUC_CHMs/",
    naip  = "Data/NAIP/HUC_NAIP_Processed/",
    ortho = "Data/Ortho/HUC_Ortho/",
    lidar = "Data/Lidar/HUC_Lidar_Metrics/"
  )
}

## --- Locate the source file for one HUC in each dataset ----------------------
## Mirrors the file filtering in the original Raster_Stack.R clust_extract_fun:
##   DEM      -> exclude whitebox ("wbt") intermediates
##   terrain  -> the local slope file, excluding 10m / 1000m scales
## Returns a named list of character vectors (length 0 = missing for this HUC).
huc_source_paths <- function(huc_number, cluster_num, dirs = huc_source_dirs()) {
  cid <- paste0("cluster_", cluster_num)
  match_one <- function(dir, extra = NULL) {
    f <- list.files(dir, pattern = "\\.tif$", full.names = TRUE)
    f <- f[grepl(huc_number, f) & grepl(cid, f)]
    if (!is.null(extra)) f <- f[extra(f)]
    f
  }
  list(
    dem   = match_one(dirs$dem,   \(f) !str_detect(f, "wbt")),
    terr  = match_one(dirs$terr,  \(f) str_detect(f, "slp") &
                                        str_detect(f, "local") &
                                        !str_detect(f, "10m|1000m")),
    hydro = match_one(dirs$hydro),
    chm   = match_one(dirs$chm),
    naip  = match_one(dirs$naip),
    ortho  = match_one(dirs$ortho),
    lidar = match_one(dirs$lidar)
  )
}

## --- Presence check ----------------------------------------------------------
## Returns TRUE only if every dataset has at least one file for this HUC.
huc_sources_ready <- function(paths, huc_number) {
  missing <- names(paths)[lengths(paths) == 0]
  if (length(missing)) {
    message("HUC ", huc_number, " missing source(s): ", paste(missing, collapse = ", "))
    return(FALSE)
  }
  TRUE
}

## --- Read + transform each source into a lazy SpatRaster ---------------------
## Order here defines the band order of the final stack (the contract):
##   DEM, terrain(slope, TPI dropped), hydro(log flowacc), CHM,
##   NAIP(ndvi/ndwi dropped), lidar
## rast() returns lazy file pointers; no pixels are read until a downstream
## crop / resample / minmax / extract forces it.
huc_layers <- function(paths) {
  pick1 <- function(x, label) {
    if (length(x) > 1) message("Multiple ", label, " files; using first: ", basename(x[1]))
    x[1]
  }

  dem <- rast(pick1(paths$dem, "DEM"))
  set.names(dem, "DEM")

  terr <- rast(pick1(paths$terr, "terrain")) 
  terr <- terra::subset(terr, grep("^TPI_", names(terr), invert = TRUE))

  hydro <- rast(pick1(paths$hydro, "hydro"))
  hydro$flowacc <- log(hydro$flowacc)

  chm <- rast(pick1(paths$chm, "CHM"))

  naip <- rast(pick1(paths$naip, "NAIP")) |>
    terra::subset(c("ndvi", "ndwi"), negate = TRUE) 
  
  ortho <- rast(pick1(paths$ortho, "Ortho"))
  names(ortho) <- paste0(names(ortho), "_lo")

  lidar <- rast(pick1(paths$lidar, "lidar"))

  # Named list, in stack order (DEM first = the reference grid).
  list(dem = dem, terr = terr, hydro = hydro, chm = chm, naip = naip, ortho = ortho, lidar = lidar)
}

## --- Align one raster to a reference grid (resample only if needed) ----------
align_to <- function(r, ref, method = "bilinear") {
  if (compareGeom(r, ref, stopOnError = FALSE)) r
  else resample(r, ref, method = method)
}

## --- Full-HUC stack (for normalization statistics) ---------------------------
## All layers aligned to the DEM grid. resample() is deferred; the caller
## materializes it by streaming, e.g. minmax(compute = TRUE). No disk write.
build_huc_stack_full <- function(paths) {
  lyrs <- huc_layers(paths)
  ref  <- lyrs$dem
  aligned <- lapply(lyrs, align_to, ref = ref)
  rast(unname(aligned))
}

## --- Per-band min/max (for normalization statistics) -------------------------
## Streams minmax() over each source raster *as read from disk* -- NO alignment
## and NO resampling. Min/max is invariant to the grid the pixels sit on, so
## there is no reason to resample every layer onto the DEM grid and hold those
## interpolated grids in memory (that is what OOMs build_huc_stack_full under
## parallel callr workers). Each source is streamed block-by-block, so memory
## stays bounded by terraOptions(memmax).
##   skip : band names to exclude (analytic/one-hot/label bands)
## Returns list(min = named-vec, max = named-vec) over all kept bands, in the
## same band order huc_layers() defines (the stack contract).
huc_minmax <- function(paths, skip = character()) {
  lyrs <- huc_layers(paths)
  mins <- list(); maxs <- list()
  for (r in lyrs) {
    keep <- setdiff(names(r), skip)
    if (length(keep) == 0) next
    mm <- minmax(r[[keep]], compute = TRUE) # streams this source block-by-block
    for (b in keep) {
      mins[[b]] <- mm["min", b]
      maxs[[b]] <- mm["max", b]
    }
  }
  list(min = unlist(mins), max = unlist(maxs))
}

## --- Patch stack (for chips and point extraction) ----------------------------
## Crop-then-resample: crop each source to the patch window (+ buffer for
## bilinear edges), then resample that small window to the DEM patch grid.
## Only patch-sized data is ever read or resampled.
##   geom      : a SpatVector defining the patch (or union of patches)
##   buffer_px : pixels of slack added around the window, sized off the coarsest
##               source (terrain) resolution, so bilinear interpolation at the
##               patch edge does not produce NA borders
##   mask      : TRUE masks all bands to the DEM crop (chip parity with the old
##               crop(stack, vect, mask = TRUE)); FALSE keeps the full window
build_huc_stack_patch <- function(paths, geom, buffer_px = 5, mask = TRUE) {
  lyrs <- huc_layers(paths)

  # DEM crop defines the target grid for this patch.
  dem_crop <- crop(lyrs$dem, geom, touches = TRUE, mask = mask)

  # Buffered window in map units, sized off the coarsest source resolution.
  e <- ext(geom)
  d <- buffer_px * res(lyrs$terr)[1]
  bwin <- ext(xmin(e) - d, xmax(e) + d, ymin(e) - d, ymax(e) + d)

  align_patch <- function(r) {
    rc <- crop(r, bwin, snap = "out")
    align_to(rc, dem_crop)
  }

  others <- lyrs[setdiff(names(lyrs), "dem")]
  # Flat list of SpatRasters (DEM first) -> rast() concatenates the layers.
  patch_lyrs <- c(list(dem_crop), lapply(unname(others), align_patch))
  out <- rast(patch_lyrs)
  if (mask) out <- terra::mask(out, dem_crop)
  out
}

## --- Optional contract guard -------------------------------------------------
## Fail loudly if a stack's bands deviate from the expected names/order. Pass
## the names captured from the first successful build of a run.
assert_band_names <- function(stack, expected) {
  got <- names(stack)
  if (!identical(got, expected)) {
    stop("Band contract violated.\n  expected: ", paste(expected, collapse = ", "),
         "\n  got:      ", paste(got, collapse = ", "))
  }
  invisible(TRUE)
}
