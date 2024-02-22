#' Split DEM into smaller tiles
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param size Size of tiles, in the same unit as the RasterLayer.
#' @param parallel Logical. Use parallel processing? Note: parallel must be installed.
#' @param ncores Number of cores to use when parallel = TRUE.
#'
#' @return List of RasterLayers.
#' @import raster
#' @export
#'
#' @examples
#' L <- habtools::extent(horseshoe) # size of horseshoe = 8m
#' size <- 2 # size of target tiles
#' (L / size)^2 # number of target tiles = 16
#' dem_list <- dem_split(horseshoe, 2)
#' length(dem_list)
#'
dem_split <- function(data,
                      size,
                      parallel = FALSE,
                      ncores = (parallel::detectCores() - 1)) {

  L <- raster::res(data)[1] * nrow(data)
  if (!round(L / size, 5) == round(L / size)) {
    message("With the specified size, the RasterLayer is not divided across the full extent. Consider a value for L0 so that L/L0 is a whole number and L0 is either equal to the resolution or a multiplicative of the resolution. ")
  }
  bb <- raster::bbox(data)
  temp <- raster::raster(xmn=bb[1,1], xmx=bb[1,2],
                         ymn=bb[2,1], ymx=bb[2,2], resolution = size,
                         crs = crs(data))

  p <- as(temp, 'SpatialPolygons')
  if (parallel) {
    parallel::mclapply(seq_along(p), function(i)
      raster::crop(data, p[i]), mc.cores = ncores)
  } else {
    lapply(seq_along(p), function(i)
      raster::crop(data, p[i]))
  }
}
