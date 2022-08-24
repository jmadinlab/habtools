#' Calculates planar area of a mesh
#'
#' @param mesh mesh object
#' @param L0 Resolution of the planar area. Is set to the resolution of the mesh when left empty.
#' @param silent Print messages and warnings
#'
#' @return A value indicating planar area
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
  m2 <- mesh
  m2$vb[3,] <- 0
  m2 <- Rvcg::vcgQEdecim(m2, edgeLength = L0, silent = T)
  Rvcg::vcgArea(m2)
}
