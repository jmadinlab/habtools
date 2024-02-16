#' Calculate extent of a 3D object
#'
#' @description This function calculates the extent or largest length of the bounding box of a mesh or a DEM.
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#'
#' @note There are several extent function is other packages, including
#' the raster package. Therefore it is recommended to use the package
#' namespace, see examples below.
#'
#' @return A value, the extent of the mesh or DEM.
#'
#' @export extent
#' @examples
#' habtools::extent(mcap)
#' habtools::extent(horseshoe)
#'
extent <- function(data){
  if (is(data, "RasterLayer")) {
    bb <- raster::extent(data)
    max(c(bb[2] - bb[1], bb[4] - bb[3]))
  } else {
    dt <- t(data$vb)
    max(c(
      abs(max(dt[,1]) - min(dt[,1])),
      abs(max(dt[,2]) - min(dt[,2])),
      abs(max(dt[,3]) - min(dt[,3]))))
  }
}
