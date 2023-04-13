#' Fractal dimension
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param x Bottom-left of bounding box. Only considered if data is a RasterLayer.
#' @param y Bottom-left of bounding box. Only considered if data is a RasterLayer.
#' @param L Bounding box extent (i.e., side length). Only considered if data is a RasterLayer.
#' @param lvec scales to use for calculation
#' @param method If data is a RasterLayer, possible methods are:"hvar", "area", and "cubes" (defaults to "hvar").
#' If data is a mesh3d, possible methods are "cubes" and "area" (defaults to "cubes").
#' @param ... Arguments from other fd_ functions.
#' @seealso [fd_hvar()]
#' @seealso [fd_area()]
#' @seealso [fd_cubes()]
#' @return A value for fractal dimension, typically between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the specified method. Note that methods are distinctly different and should not be mixed when comparing values for multiple objects.
#' The `cubes` method is not recommended if the height range is much smaller than the extent of a 3d object or DEM, which is typically the case for DEMs.
#'
#' @examples
#' library(habtools)
#' fd(horseshoe, method = "hvar", x = -470, y = 1266, L = 2, lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd(horseshoe, method = "area", x = -470, y = 1266, L = 2, lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd(mcap, method = "cubes", lvec = c(0.045, 0.09, 0.18, 0.5), plot = TRUE)
#' fd(mcap, method = "area", lvec = c(0.02, 0.04, 0.08, 0.16))
#'
fd <- function(data,  method, x, y, L, lvec, ...) {

  if (class(data) == "RasterLayer") {

    # subset raster if needed
    if (missing(x)) x <- raster::xmin(data)
    if (missing(y)) y <- raster::ymin(data)
    if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))
    if (L < min(dim(data)[1:2] * raster::res(data))) {
      b <- as(raster::extent(x, x + L, y, y + L), 'SpatialPolygons')
      raster::crs(b) <- raster::crs(data)
      data <- raster::crop(data, b)
    }

    if (missing(method)) method <- "hvar"

    # calculate fd
    if (method == "hvar") {
      f <- fd_hvar(data, lvec = lvec, ...)
    } else if (method == "area") {
      f <- fd_area(data, lvec = lvec, ...)
    } else if (method == "cubes") {
      f <- fd_cubes(data, lvec = lvec, ...)
    } else {
      stop("Please check appropriate method options.")
    }

  } else if (class(data) == "mesh3d") {
    if (missing(method)) method <- "cubes"
    if (method == "cubes") {
      f <- fd_cubes(data, lvec = lvec, ...)
    } else if (method == "area") {
      f <- fd_area(data, lvec = lvec, ...)
    } else {
      stop("Please check appropriate method options.")
    }
  }
  return(f)
}
