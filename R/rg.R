#' Rugosity
#'
#' @param data Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param L0 Grain or resolution of calculation.
#' @param plot Add bounding box to plot.
#' @param method "mesh" or "dem"
#'
#' @return
#' @export
#'
#' @examples
#' rg(horseshoe, x=-470, y=1266, L=2, L0=0.5)
#'
#' plot(horseshoe, asp=1)
#' rg(horseshoe, x=-470, y=1266, L=2, L0=0.5, plot=TRUE)
#'
#' rg(mcap, L0 = 0.01, method = "mesh")
#'
rg <- function(data, x, y, L, L0, method="dem", plot=FALSE) {
  if (method =="dem") {
    if (missing(x)) x <- raster::xmin(data)
    if (missing(y)) y <- raster::ymin(data)
    if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))

    cells <- expand.grid(x = seq(x, x + L - L0, L0),
                         y = seq(y, y + L - L0, L0))
    hs <- mapply(hr, x = cells$x, y = cells$y, MoreArgs =
                   list(L = L0, data = data, plot = plot))
    H0 <- mean(hs)
    rg <- sqrt((H0^2) / (2 * L0^2) + 1)
  }
  if (method =="mesh") {
    res <- Rvcg::vcgMeshres(data)$res[[1]]
    if (L0 < res) {
      warning("L0 is smaller than mesh resolution")
    }
    mesh <- Rvcg::vcgQEdecim(data, edgeLength = L0)
    area <- Rvcg::vcgArea(mesh)
    planar <- (max(data$vb[1,]) - min(data$vb[1,])) * (max(data$vb[2,]) - min(data$vb[2,]))
    rg <- area/planar
  }
  return(rg)
}

