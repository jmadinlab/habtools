#' Transform mesh to 3D pointcloud
#'
#' @param mesh A triangular mesh of class mesh3d.
#'
#' @return A data frame with XYZ coordinates.
#' @export
#'
#'

mesh_to_points <- function(mesh) {
  out <- data.frame(t(mesh$vb)[,1:3])
  colnames(out) <- c("x", "y", "z")
  out
}
