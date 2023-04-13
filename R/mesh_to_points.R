#' Transform DEM to 3D pointcloud of raster corners
#'
#' @param mesh A digital elevation model in raster format
#'
#' @return A 3D point cloud
#' @export
#'
#'

mesh_to_points <- function(mesh) {
  out <- data.frame(t(mesh$vb)[,1:3])
  colnames(out) <- c("x", "y", "z")
  out
}
