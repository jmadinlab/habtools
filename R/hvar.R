#' Height Variation in cells at different scales
#'
#' @param dem Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param Lvec A vector of scales to collate.
#'
#' @return A `data.frame` containing height ranges of cells at different scales.
#' @export
#'
#' @details
#' This function is used for calculating fractal dimension using the
#' height variation method.
#' It is separated from the `FD` function because it takes a while to run, and so it is useful
#' to save the output before using the `FD` function.
#'
#' @examples
#'
#' hvar(horseshoe, x=-470, y=1266, L=2, Lvec=c(2, 1, 0.5))

hvar <- function(dem, x, y, L, Lvec) {
  hvar <- data.frame()
  for (L0 in Lvec) {
    cells <- expand.grid(x=seq(x, x+L-L0, L0), y=seq(y, y+L-L0, L0))
    H0 <- mapply(hr, x=cells$x, y=cells$y, MoreArgs=list(L=L0, data=dem))
    hvar <- rbind(hvar, data.frame(L0=L0, H0=H0))
  }
  return(hvar)
}
