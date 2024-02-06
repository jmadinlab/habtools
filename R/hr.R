#' Height range
#'
#' @description Calculates the distance between the lowest and highest point in a 3D object.
#'
#' @param data A RasterLayer or mesh3d object.
#'
#' @return The difference between the lowest and highest point.
#'
#' @export
#'
#' @examples
#' # for a DEM
#' raster::plot(habtools::horseshoe)
#' hr(horseshoe)
#'
#' # for a 3D mesh
#' hr(mcap)
#'

hr <- function(data) {


  if (is(data, "RasterLayer")) {


    if (sum(is.na(values(data))) > 0) {
      message(paste0("data contains ", sum(is.na(values(data))), " NA values. Results may be biased."))
    }


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
