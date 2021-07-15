#' Height range
#'
#' @param dem A digital elevation model (DEM) in raster format.
#' @param x The lower-left x-coordinate of bounding box.
#' @param y The lower-left y-coordinate of bounding box.
#' @param L The bounding box size (i.e., side length).
#' @param plot Add bounding box to existing plot of `dem`.
#'
#' @return The difference between the lowest and highest point in the
#' bounding box.
#'
#' @export
#'
#' @examples
#' hr(horseshoe, -470, 1266, 2)
#'
#' plot(horseshoe)
#' hr(horseshoe, -470, 1266, 2, plot=TRUE)
#'
hr <- function(dem, x, y, L, plot=FALSE) {
  ext <- raster::extent(cbind(c(x, y), c(x + L, y + L)))
  hr <- diff(range(raster::getValues(raster::crop(dem, ext)), na.rm=TRUE))
  if (plot) rect(x, y, x + L, y + L)
  return(hr)
}
