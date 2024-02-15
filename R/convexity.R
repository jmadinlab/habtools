#' Convexity
#'
#' @description
#' Calculates the convexity of a 3D mesh.
#'
#' @param mesh A triangular mesh of class mesh3d.
#'
#' @details The ratio of the volume of the object and the volume of the convex
#' hull around the object. Objects with fewer concavities will be closer to 1.
#'
#' @return The convexity value.
#' @export
#'
#' @examples
#' convexity(mcap)

convexity <- function(mesh){
  vol <- suppressWarnings(Rvcg::vcgVolume(mesh))
  pts <- mesh_to_points(mesh)
  cvol <- geometry::convhulln(pts, options = "FA")[[3]]
  vol <- suppressWarnings(Rvcg::vcgVolume(mesh))
  vol/cvol
}
