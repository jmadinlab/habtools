#' Calculate convexity
#'
#' @param mesh mesh3d object
#'
#' @details The ratio of the volume of the object and the volume of the convex hull around the object
#'
#' @return the convexity value
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
