#' Calculates planar area of a mesh
#'
#' @param mesh A triangular mesh of class mesh3d.
#' @param L0 Resolution of the planar area. Is set to the resolution of the mesh when left empty.
#' @param silent Logical. Suppress messages and warnings?
#'
#' @return A value for planar area.
#' @export
#'
#' @examples
#' planar(mcap)
planar <- function(mesh, L0, silent = FALSE) {
  res <- Rvcg::vcgMeshres(mesh)$res[[1]]
  if (missing(L0)){
    L0 <- res
    if (!silent){
      message(paste("L0 is set to mesh resolution (", L0, ")", sep = ""))
    }
  }
  if (L0 < res) {
    if (!silent){
      warning("L0 is smaller than mesh resolution")
    }
  }
  if (L0 > res) {
    mesh <- Rvcg::vcgQEdecim(mesh, edgeLength = L0, silent = TRUE)
  }
  # get normals of faces
  m <- mesh
  n <- Rvcg::vcgFaceNormals(mesh)
  # remove faces facing downward
  t <- which(n[3,]<0)
  m$it <- m$it[,-t]
  m$vb[3,] <- 0
  m <- Rvcg::vcgQEdecim(m, edgeLength = L0, silent = TRUE)
  x <- m$vb[1,]
  y <- m$vb[2,]
  dt <- data.frame(x =x, y = y)
  poly <- concaveman::concaveman(as.matrix(dt), concavity = 1, length_threshold = L0)

  geometry::polyarea(poly[,1], poly[,2])
}
