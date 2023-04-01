#' Rescale mesh based on a fixed volume of 1
#'
#' @param mesh A 3D mesh object
#'
#' @return A mesh with volume = 1
#' @export
#'
#' @examples
#' Rvcg::vcgVolume(mcap)
#' mcap_scaled <- scale_volume(mcap)
#' Rvcg::vcgVolume(mcap_scaled)

scale_volume <- function(mesh){
  vol <- suppressWarnings(Rvcg::vcgVolume(mesh))
  XYZCoords <- t(mesh$vb[1:3,])
  XYZCoordsTwo <- XYZCoords/(vol^(1/3))
  mesh_scaled <- mesh
  mesh_scaled$vb[1:3,] <- t(XYZCoordsTwo)
  return(mesh_scaled)
}
