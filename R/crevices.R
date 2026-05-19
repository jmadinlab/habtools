# =============================================================================
#  crevices.R
#
#  Detect crevice / pit features on reef DEMs by comparing two median-smoothed
#  surfaces, thresholding the residual height, and filtering small patches.
#
#  Original ArcGIS Pro Spatial Analyst pipeline this is ported from:
#    1. Focal Median (fine kernel)   - optional denoising of input DEM
#    2. Focal Median (coarse kernel) - broad-scale "envelope" surface
#    3. Raster Calculator            - residual = coarse - fine  (gap depth)
#    4. Con (lo <= gap <= hi)        - keep cells whose depth lies in band
#    5. Majority Filter (3x3, HALF)  - clean up speckle in the binary mask
#    6. Region Group (8-connected)   - label connected components
#    7. Extract by Attributes (>N)   - drop patches smaller than threshold
#    8. Con(IsNull)                  - re-binarise (0/1)
#    9. Extract by Mask (OUTSIDE)    - subtract a coral-cover polygon
# =============================================================================


#' Detect crevices in a DEM
#'
#' Identifies crevice / pit features in a digital elevation model. The DEM is
#' compared against a coarser median-smoothed copy of itself; cells whose
#' residual height falls inside \code{gap_range} are flagged, cleaned with a
#' majority filter, grouped into 8-connected patches, and filtered by area.
#'
#' Window sizes (\code{L0}, \code{L1}) and area thresholds (\code{min_area})
#' are specified in **map units** (metres for projected DEMs). The function
#' inspects the DEM's resolution and converts these scales to cell counts at
#' run time, so the same call works on DEMs of different resolutions.
#'
#' @param data DEM as a \code{RasterLayer} (\pkg{raster}) or \code{SpatRaster}
#'   (\pkg{terra}). Should be in a projected CRS with linear units (e.g. UTM).
#' @param L0 Fine-scale denoising window, in map units. Optional. Use only
#'   when the DEM carries appreciable per-cell noise (typical for SfM
#'   reconstructions). Set to \code{NULL} (default) to keep raw detail.
#'   A typical noisy-DEM value is \code{0.005} (5 mm).
#' @param L1 Coarse-scale envelope window, in map units. Default
#'   \code{0.030} (30 mm). Should be substantially larger than \code{L0}
#'   and larger than the crevices you want to detect.
#' @param gap_range Length-2 numeric \code{c(lo, hi)}, in map units. Cells
#'   whose residual height (\code{coarse - fine}) lies in \code{[lo, hi]}
#'   are flagged as candidate crevices. Default \code{c(0.003, 0.05)}.
#' @param min_area Minimum patch area to retain, in map units squared.
#'   Default \code{1e-4} (= 100 mm^2, i.e. 100 cells at 1 mm/pix).
#' @param mask Optional polygon (\code{sf}, \code{SpatVector}, or
#'   \code{Spatial*}). Cells inside the polygon are removed from the output,
#'   matching ArcGIS "Extract by Mask, OUTSIDE".
#' @param binary Logical. If \code{TRUE} (default) the output is a 0/1
#'   raster; if \code{FALSE} retained patches keep their integer labels.
#' @param verbose Logical. Print resolution diagnostics and effective
#'   window sizes (default \code{TRUE}).
#'
#' @return A raster of the same class as \code{data}.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   dem <- rast("dome_dem.tif")        # 1 mm/pix UTM DEM
#'   cr  <- crevices(dem)               # use defaults, no fine denoising
#'   cr2 <- crevices(dem, L0 = 0.005)   # enable 5 mm median denoising
#'   plot(cr)
#' }
crevices <- function(data,
                     L0        = NULL,
                     L1        = 0.030,
                     gap_range = c(0.003, 0.05),
                     min_area  = 1e-4,
                     mask      = NULL,
                     binary    = TRUE,
                     verbose   = TRUE) {

  # ---------------------------------------------------------------------------
  # Input handling: accept either RasterLayer (raster pkg) or SpatRaster
  # (terra pkg). Internally we work in terra; we remember the input class so
  # we can return the same type the user passed in.
  # ---------------------------------------------------------------------------
  input_class <- class(data)[1]
  if (inherits(data, "RasterLayer")) {
    r <- terra::rast(data)
  } else if (inherits(data, "SpatRaster")) {
    r <- data
  } else {
    stop("`data` must be a RasterLayer or SpatRaster.")
  }

  # Validate the gap-range band early; this is the most common user error.
  if (length(gap_range) != 2 || gap_range[1] >= gap_range[2]) {
    stop("`gap_range` must be a length-2 numeric with gap_range[1] < gap_range[2].")
  }

  # ---------------------------------------------------------------------------
  # Resolution diagnostics. Reads the DEM's pixel size and CRS, prints a short
  # summary, and returns the values we need to convert physical scales to
  # cell counts. Also warns if the DEM is unprojected (lon/lat).
  # ---------------------------------------------------------------------------
  info  <- .dem_resolution(r, verbose = verbose)
  res_x <- info$res_x
  res_y <- info$res_y

  # Convert a physical scale (map units) to an odd cell count. Focal kernels
  # need odd dimensions so the centre cell is well defined; we round to the
  # nearest cell and bump even values up by one.
  to_cells <- function(L, res) {
    n <- max(1, round(L / res))
    if (n %% 2 == 0) n <- n + 1
    n
  }

  # Coarse window in cells, and minimum patch size in cells (area / cell_area).
  w1        <- to_cells(L1, res_x)
  min_cells <- max(1, round(min_area / (res_x * res_y)))

  # ---------------------------------------------------------------------------
  # Step 1: Fine-scale denoising (OPTIONAL).
  #
  # When L0 is NULL we skip this step and treat the raw DEM as `fine`. When
  # L0 is given but resolves to fewer than 3 cells (e.g. user asked for a
  # 5 mm filter on a 5 mm/pix DEM), the kernel is degenerate and we also
  # skip; in both cases a message is emitted so the choice is transparent.
  # ---------------------------------------------------------------------------
  if (is.null(L0)) {
    if (verbose) message("L0 = NULL: skipping fine-scale denoising; ",
                         "using raw DEM as the fine surface.")
    fine <- r
  } else {
    w0 <- to_cells(L0, res_x)
    if (w0 < 3) {
      if (verbose) message(sprintf(
        "L0 = %g %s resolves to %d cell(s); too small to denoise - skipping.",
        L0, info$unit, w0))
      fine <- r
    } else {
      if (verbose) message(sprintf(
        "L0 = %g %s -> %d-cell median filter (denoising).",
        L0, info$unit, w0))
      fine <- terra::focal(r, matrix(1, w0, w0),
                           fun = "median", na.rm = TRUE)
    }
  }

  if (verbose) {
    message(sprintf(
      "L1 = %g %s -> %d-cell median filter (broad surface).",
      L1, info$unit, w1))
    message(sprintf(
      "Min patch size: %g %s^2 -> %d cells.",
      min_area, info$unit, min_cells))
    if (w1 <= 3)
      warning("L1 resolves to a tiny window; the 'coarse' surface will be ",
              "almost identical to the fine one. Consider increasing L1.")
  }

  # ---------------------------------------------------------------------------
  # Step 2: Coarse "envelope" surface.
  # A wide median filter recovers the broad shape of the substrate, ignoring
  # narrow depressions. The residual (coarse - fine) is then a depth map of
  # those depressions.
  # ---------------------------------------------------------------------------
  coarse <- terra::focal(fine, matrix(1, w1, w1),
                         fun = "median", na.rm = TRUE)

  # ---------------------------------------------------------------------------
  # Step 3-4: Residual + threshold band.
  # Cells whose depression depth falls inside [gap_range[1], gap_range[2]]
  # are flagged as 1; everything else becomes NA so it does not interfere
  # with the subsequent connected-component step.
  # ---------------------------------------------------------------------------
  gap     <- coarse - fine
  flagged <- terra::ifel(gap >= gap_range[1] & gap <= gap_range[2], 1, NA)

  if (all(is.na(terra::values(flagged)))) {
    gap_vals <- terra::values(gap)
    gap_vals <- gap_vals[!is.na(gap_vals)]
    warning("No cells fall within gap_range [", gap_range[1], ", ", gap_range[2], "]. ",
            "Actual gap depth range: [", round(min(gap_vals), 5), ", ",
            round(max(gap_vals), 5), "]. ",
            "Adjust `gap_range` accordingly.")
    out <- terra::ifel(is.na(r), NA, 0)
    if (input_class == "RasterLayer") return(raster::raster(out)) else return(out)
  }

  # ---------------------------------------------------------------------------
  # Step 5: Majority filter (3x3, "HALF" rule).
  # Cleans up speckle in the binary mask. For each cell, look at its eight
  # neighbours; if the most common neighbour value occurs in at least half of
  # the (non-NA) neighbours, replace the centre with that value, otherwise
  # keep the centre unchanged. Implemented as a small focal callback below.
  # ---------------------------------------------------------------------------
  smoothed <- terra::focal(flagged, matrix(1, 3, 3), fun = .majority_half)

  # ---------------------------------------------------------------------------
  # Step 6-7: Connected-component labelling and size filter.
  # `patches()` assigns every contiguous run of non-NA cells a unique integer
  # label (8-neighbour connectivity). We then count cells per label via
  # `freq()` and keep only labels whose cell count exceeds `min_cells`.
  # ---------------------------------------------------------------------------
  patches  <- terra::patches(smoothed, directions = 8, zeroAsNA = TRUE)
  cnt      <- terra::freq(patches)             # cols: layer, value, count
  keep     <- cnt$value[cnt$count > min_cells]

  if (length(keep) == 0) {
    warning("No patches survived the area filter (min_area = ", min_area,
            " -> ", min_cells, " cells). ",
            "Try lowering `min_area` or widening `gap_range`.")
    out <- terra::ifel(is.na(r), NA, 0)
    if (input_class == "RasterLayer") return(raster::raster(out)) else return(out)
  }

  retained <- terra::ifel(patches %in% keep, patches, NA)

  # ---------------------------------------------------------------------------
  # Step 8: Optional binarisation.
  # Default behaviour matches the ArcGIS pipeline: 1 inside retained patches,
  # 0 elsewhere. Set binary = FALSE to keep the integer patch labels, which
  # is useful for per-patch statistics downstream.
  # ---------------------------------------------------------------------------
  out <- if (binary) terra::ifel(is.na(retained), 0, 1) else retained

  # ---------------------------------------------------------------------------
  # Step 9: Optional polygon mask (OUTSIDE).
  # Removes cells that fall inside the supplied polygon (e.g. live coral
  # cover) and crops the result to that polygon's extent. CRS is reprojected
  # if it differs from the DEM.
  # ---------------------------------------------------------------------------
  if (!is.null(mask)) {
    mv <- terra::vect(mask)
    if (!terra::same.crs(out, mv)) mv <- terra::project(mv, terra::crs(out))
    out <- terra::mask(out, mv, inverse = TRUE)
    out <- terra::crop(out, terra::ext(mv))
  }

  # Return the same class the caller passed in.
  if (input_class == "RasterLayer") raster::raster(out) else out
}


# =============================================================================
#  Internal helpers (not exported; prefix with "." per habtools convention)
# =============================================================================

#' Inspect DEM resolution and CRS
#'
#' Prints a compact diagnostic block describing the DEM (CRS, lon/lat flag,
#' pixel size in convenient units, dimensions). Returns the raw resolution
#' values used elsewhere in the pipeline.
#'
#' @keywords internal
.dem_resolution <- function(r, verbose = TRUE) {

  res_x     <- terra::xres(r)
  res_y     <- terra::yres(r)
  crs_obj   <- terra::crs(r, describe = TRUE)
  is_lonlat <- terra::is.lonlat(r)
  unit      <- if (isTRUE(is_lonlat)) "deg" else "m"

  # Pretty-print the resolution at the most readable scale (mm / cm / m).
  if (unit == "m") {
    res_mm <- res_x * 1000
    res_str <- if (res_mm < 10)
      sprintf("%.3f mm/pix", res_mm)
    else if (res_x < 1)
      sprintf("%.2f cm/pix", res_x * 100)
    else
      sprintf("%.3f m/pix", res_x)
  } else {
    res_str <- sprintf("%.6f deg/pix", res_x)
  }

  if (verbose) {
    message("---- DEM info ----")
    message("CRS         : ", crs_obj$name %||% "unknown",
            if (!is.na(crs_obj$code)) sprintf(" (EPSG:%s)", crs_obj$code) else "")
    message("Lon/lat     : ", isTRUE(is_lonlat))
    message("Resolution  : ", res_str,
            if (abs(res_x - res_y) > 1e-12)
              sprintf(" (x=%g, y=%g; non-square)", res_x, res_y) else "")
    message("Dimensions  : ", terra::nrow(r), " x ", terra::ncol(r),
            " (", terra::ncell(r), " cells)")
    message("------------------")
  }

  # Hard warning if the DEM is unprojected: physical-scale parameters would
  # be silently interpreted in degrees, which is almost never what the user
  # wants for sub-metre crevice detection.
  if (isTRUE(is_lonlat))
    warning("DEM appears to be in geographic (lon/lat) coordinates. ",
            "`L0`, `L1`, `gap_range`, `min_area` are interpreted in degrees. ",
            "Reproject to a metric CRS (e.g. UTM) for sensible results.")

  list(res_x = res_x, res_y = res_y,
       unit  = unit,  is_lonlat = isTRUE(is_lonlat))
}


#' 3x3 majority filter with the "HALF" rule
#'
#' For a 3x3 neighbourhood (passed by \code{terra::focal} as a length-9
#' vector in row-major order, centre at index 5):
#'   - Drop NA neighbours.
#'   - If the most frequent neighbour value occurs in at least half of the
#'     remaining neighbours, return that value.
#'   - Otherwise return the centre value unchanged.
#'
#' @keywords internal
.majority_half <- function(x, ...) {
  centre <- x[5]
  nb     <- x[-5]
  nb     <- nb[!is.na(nb)]
  if (!length(nb)) return(centre)
  uq  <- unique(nb)
  tab <- tabulate(match(nb, uq))
  if (max(tab) >= length(nb) / 2) uq[which.max(tab)] else centre
}


# Null/empty-coalesce operator: returns `b` if `a` is NULL, NA, or empty.
# Used for tidy fallback when CRS metadata is missing.
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || !nzchar(a)) b else a