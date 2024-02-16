#' Calculate packing of 3D object
#'
#' @description
#' The ratio of the surface area of the object and the surface area of the convex hull around the object.
#'
#' @param mesh A triangular mesh of class mesh3d.
#'
#' @return Value of packing.
#' @export
#'
#' @examples
#' packing(mcap)


packing <- function(mesh){
  pts <- mesh_to_points(mesh)
  csa <- geometry::convhulln(pts, options = "FA")$area
  sa <- Rvcg::vcgArea(mesh)
  sa/csa
}
