#' Height range
#'
#' @description Calculates the distance between the lowest and highest point in a 3D object.
#'
#' @param data A digital elevation model (DEM) in a "RasterLayer" class or a "mesh3d" object.
#'
#' @return The difference between the lowest and highest point.
#'
#' @export
#'
#' @examples
#'
#' library(raster)
#' plot(horseshoe)
#' hr(horseshoe)
#'
#' # For a 3D mesh.
#' hr(mcap)
#'
hr <- function(data) {


  if (is(data, "RasterLayer")) {

    out <- diff(range(values(data), na.rm = TRUE))

  } else if (is(data, "data.frame")) {

    out <- max(data[, 3], na.rm = TRUE) - min(data[, 3], na.rm = TRUE)

  } else if (is(data, "mesh3d")) {
    pts <- data.frame(t(data$vb)[,1:3])
    names(pts) <- c("x", "y", "z")

    out <- diff(range(pts$z, na.rm = TRUE))
  } else {
    stop("data must be either an object of class RasterLayer, data.frame, or mesh3d")
  }
  return(out)
}
