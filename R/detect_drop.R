#' Detect a sudden drop, edge, or overhang in a DEM
#'
#' @param data DEM of class RasterLayer.
#' @param d The threshold height difference to define a drop.
#'
#' @return A RasterLayer marking edges. Values indicate maximum height difference of surrounding cells
#' @export
#'
#' @examples
#' library(raster)
#' library(habtools)
#' dem <- detect_drop(horseshoe, d = 0.2)
#' plot(horseshoe)
#' plot(dem)
#'

detect_drop <- function(data, d = 0.1) {
  fy <- matrix(c(0,0,0,1,0,-1,0,0,0), nrow=3)
  fx <- matrix(c(0,-1,0,0,0,0,0,1,0) , nrow=3)
  d1 <- matrix(c(1,0,0,0,0,0,0,0,-1), nrow=3)
  d2 <- matrix(c(0,0,1,0,0,0,-1,0,0) , nrow=3)
  rx <- abs(raster::focal(data, fx))
  ry <- abs(raster::focal(data, fy))
  rd1 <- abs(raster::focal(data, d1))
  rd2 <- abs(raster::focal(data, d2))
  out <- max(rx, ry, rd1, rd2)
  out <- out * (out > d)
  out@data@values[out@data@values<d] <- NA
  out
}
