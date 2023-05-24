#' Surface area
#'
#' Calculates surface area
#'
#' @param data DEM in RasterLayer format or mesh3d object
#'
#' @return surface area
#' @export
#'
#' @examples
#' surface_area(mcap)
#' surface_area(horseshoe)

surface_area <- function(data) {
  if (is(data, "RasterLayer")) {
    L0 <- res(data)[1]
    mat <- as.matrix(data)/L0
    sp::surfaceArea(mat)*(L0^2)
  } else if (is(data, "mesh3d")) {
    Rvcg::vcgArea(data)
  } else {
    stop("data must be of class RasterLayer or mesh3d with triangular mesh")
  }
}


