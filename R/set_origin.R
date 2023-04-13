#' Set the origin of a mesh
#'
#' Shift the values of a set of XYZ coordinates so that the
#' origin lies at the reference vertex (defaults to the minimum of x, y, and z coordinates)
#' Title
#'
#' @param mesh mesh3d object
#' @param reference Vector containing coordinates of the reference vertex.
#' If left empty, this will default to the minimum of x, y, and z.
#'
#' @return mesh3d object
#' @export
#'
#' @examples
#' mesh <- set_origin(mcap)

set_origin <- function(mesh, reference = NULL) {
  XYZCoords <- t(mesh$vb[1:3,])

  if (is.null(reference)) {
    reference[1] <- min(XYZCoords[,1])
    reference[2] <- min(XYZCoords[,2])
    reference[3] <- min(XYZCoords[,3])
  }

  XYZCoords[,1] <- XYZCoords[,1] - (reference[1])
  XYZCoords[,2] <- XYZCoords[,2] - (reference[2])
  XYZCoords[,3] <- XYZCoords[,3] - (reference[3])
  mesh_scaled <- mesh
  mesh_scaled$vb[1:3,] <- t(XYZCoords)
  return(mesh_scaled)
}
