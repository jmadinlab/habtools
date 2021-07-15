#' Rugosity
#'
#' @param dem Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param L0 Grain or resolution of calculation.
#' @param plot Add bounding box to plot.
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
rg <- function(dem, x, y, L, L0, plot=FALSE) {
  cells <- expand.grid(x=seq(x, x+L-L0, L0), y=seq(y, y+L-L0, L0))
  H0 <- mapply(hr, x=cells$x, y=cells$y, MoreArgs=list(L=L0, dem=dem, plot=plot))
  H0 <- mean(H0)
  rg <- sqrt((H0^2) / (2 * L0^2) + 1)
  return(rg)
}
