#' Calculate rugosity, fractal dimension and height for a DEM
#'
#' @param data A dem of class RasterLayer
#' @param x minimum x-value
#' @param y minimum y-value
#' @param L Extent
#' @param lvec Scales to include in fd calculation see [fd()]
#' @param ... Additional arguments see [fd()]
#'
#' @seealso [fd()]
#' @seealso [rg()]
#' @seealso [hr()]
#'
#' @details uses hvar method for rugosity and fractal dimension calculations.
#' @return a dataframe with the three complexity measures
#' @export
#'
#' @examples
#' rdh(horseshoe, x = -470, y = 1266, L = 2, lvec = c(0.125, 0.25, 0.5, 1, 2))
rdh <- function(data, x, y, L, lvec, ...){

  if (missing(x)) x <- raster::xmin(data)
  if (missing(y)) y <- raster::ymin(data)
  if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))
  if (L < min(dim(data)[1:2] * raster::res(data))) {
    b <- as(raster::extent(x, x + L, y, y + L), 'SpatialPolygons')
    raster::crs(b) <- raster::crs(data)
    data <- raster::crop(data, b)
  }

  hv <- hvar(data, lvec = lvec, ...)
  hs <- hv[hv$l == min(hv$l, na.rm = T), "h"]
  rg <- mean(sqrt((hs^2) / (2 * min(hv$l, na.rm = T)^2) + 1), na.rm = T)
  d <- fd_hvar(hv)
  h <- hr(data)

  data.frame(R = rg, D = d, H = h)
}