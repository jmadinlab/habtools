#' Calculate packing
#'
#' @param mesh A mesh3D object
#'
#' @details The ratio of the surface area of the object and the surface area of the convex hull around the object.
#' @return value
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
