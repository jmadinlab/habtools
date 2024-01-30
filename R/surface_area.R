#' Surface area
#'
#' Calculates surface area
#'
#' @param data DEM in RasterLayer format, mesh3d object or data frame with xy coordinates
#'
#' @return surface area
#' @export
#'
#' @examples
#' surface_area(mcap)
#' surface_area(horseshoe)
#' surface_area(mesh_to_2d(mcap))
#'

surface_area <- function(data) {
  if (is(data, "RasterLayer")) {
    L0 <- res(data)[1]
    mat <- as.matrix(data)/L0
    sp::surfaceArea(mat)*(L0^2)
  } else if (is(data, "mesh3d")) {
    Rvcg::vcgArea(data)
  } else if (is(data, "data.frame")) {
    geometry::polyarea(data[,1], data[,2])
  } else {
    stop("Data must be of class RasterLayer, mesh3d with triangular mesh, or a data frame with xy coordinates")
  }
}

