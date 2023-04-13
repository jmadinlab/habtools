#' Re-scale mesh based on a fixed area
#'
#' @param mesh A 3D mesh object
#' @param target_area The target area of the scaled 3D mesh. Defaults to 1.
#'
#' @return A mesh with area = target_area (1 as default).
#' @export
#'
#' @examples
#' Rvcg::vcgArea(mcap)
#' mcap_scaled <- scale_area(mcap)
#' Rvcg::vcgArea(mcap_scaled)

scale_area <- function(mesh, target_area = 1) {
  XYZCoords <- t(mesh$vb[1:3,])
  area <- Rvcg::vcgArea(mesh)
  XYZCoordsTwo <- (XYZCoords/((area^(1/2)/(target_area^(1/2)))))
  mesh_scaled <- mesh
  mesh_scaled$vb[1:3,] <- t(XYZCoordsTwo)
  return(mesh_scaled)
}
