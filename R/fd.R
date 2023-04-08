#' fractal dimension
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param Lvec scales to use for calculation
#' @param method If data is a RasterLayer methods "hvar" or "area" are allowed. Defaults to "hvar".
#' @param parallel Use parallel processing (Defaults to FALSE)
#' @param ncores number of cores to use if parallel = T.
#' (Defaults to umber of available cores - 1)
#' @return A value for fractal dimension, hopefully between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the height variation method.
#' `data` can be any `data.frame` with columns labeled `l` and `h` for
#' grid cell length and height range of that cell, respectively.
#' A rule of thumb is that `l` should range two orders of magnitude.
#' However, large ranges also
#' average-out fractal dimension of a surface that might have
#' phase transitions, and therefore a thorough exploration of height ranges is suggested using the `diagnostic` plot, which plots all the methods.
#' These methods will converge on purely fractal surfaces.
#'
#' @examples
#' fd(horseshoe, method = "hvar", x=-470, y=1266, L=2, Lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd(data)
#' fd(data, method = "ends")
#' fd_hvar(data, method = "median", plot = TRUE)
#'
fd <- function(data,  method, x, y, L, Lvec) {

  if (method == "hvar") {
    h <- hvar(data, x = x, y = y, L = L, Lvec = Lvec)
    fd <- fd_hvar(h, plot = T)
  } else if (method == "area") {
    fd <- "a"
  } else if (method == "cubes") {
    fd <- "c"
  } else {
    stop("Please check method options.")
  }
  return(as.vector(fd))
}
