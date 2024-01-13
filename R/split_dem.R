#' Split DEM into smaller tiles
#'
#' @param data RasterLayer
#' @param size size of tiles, relative to the unit of the RasterLayer
#' @param parallel TRUE or FALSE. Use parallel processing? Note: parallel must be installed.
#' @param ncores number of cores to use when parallel = TRUE.
#'
#' @return List of RasterLayers
#' @import raster
#' @export
#'
#' @examples
#' L <- raster::res(horseshoe)[1] * nrow(horseshoe) # size of horseshoe = 8m
#' size <- 2 # size of target tiles
#' (L / size)^2 # number of target tiles = 16
#' dem_list <- split_dem(horseshoe, 2)
#' length(dem_list)
#' sapply(dem_list, hr) # apply height range calculation on each tile
#'
split_dem <- function(data,
                      size,
                      parallel = FALSE,
                      ncores = (parallel::detectCores() - 1)) {
  L <- raster::res(data)[1] * nrow(data)
  if (!round(L / size, 5) == round(L / size)) {
    message("With the specified size, the RasterLayer is not divided across the full extent. Consider a value for L0 so that L/L0 is a whole number and L0 is either equal to the resolution or a multiplicative of the resolution. ")
  }
  t <- nrow(data) / (L / size)
  a <- raster::aggregate(data, t)
  p <- as(a, 'SpatialPolygons')
  if (parallel) {
    parallel::mclapply(seq_along(p), function(i)
      raster::crop(data, p[i]), mc.cores = ncores)
  } else {
    lapply(seq_along(p), function(i)
      raster::crop(data, p[i]))
  }
}
