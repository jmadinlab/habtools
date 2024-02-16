#' Count filled cells 3D
#'
#' @description
#' A helper function for segment, box and cube counting fractal methods.
#' The function divide the array into `n` pieces and counts how many are
#' occupied.
#'
#' @param pts Data frame with x, y, and z coordinates
#' @param xmin Minimum x-value
#' @param xmax Maximum x-value
#' @param ymin Minimum y-value
#' @param ymax Maximum y-value
#' @param zmin Minimum z-value
#' @param zmax Maximum z-value
#' @param n Multiplier
#'
#' @return Number of filled cells
#' @export
#'

cell_count_3d <- function(pts, xmin, xmax, ymin, ymax, zmin, zmax, n) {
  x <- rescale(pts$x, xmin, xmax, n)
  x[x==0] <- 1
  y <- rescale(pts$y, ymin, ymax, n)
  y[y==0] <- 1
  z <- rescale(pts$z, zmin, zmax, n)
  z[z==0] <- 1
  cnt <- table(ceiling(x), ceiling(y), ceiling(z))
  sum(cnt > 0)
}

#' Count filled cells in 2D
#'
#' @description
#' A helper function for segment, box and cube counting fractal methods.
#' The function divide the array into `n` pieces and counts how many are
#' occupied.
#'
#' @param pts Data frame with x and y coordinates
#' @param xmin Minimum x-value
#' @param xmax Maximum x-value
#' @param ymin Minimum y-value
#' @param ymax Maximum y-value
#' @param n Multiplier
#'
#' @return Number of filled cells
#' @export
#'

cell_count_2d <- function(pts, xmin, xmax, ymin, ymax, n) {
  x <- rescale(pts$x, xmin, xmax, n)
  x[x==0] <- 1
  y <- rescale(pts$y, ymin, ymax, n)
  y[y==0] <- 1
  cnt <- table(ceiling(x), ceiling(y))
  sum(cnt > 0)
}

#' Count filled cells in 1D
#'
#' @description
#' A helper function for segment, box and cube counting fractal methods.
#' The function divide the array into `n` pieces and counts how many are
#' occupied.
#'
#' @param pts Data frame with x coordinates
#' @param xmin Minimum x-value
#' @param xmax Maximum x-value
#' @param n Multiplier
#'
#' @return Number of filled cells
#' @export
#'
#' @examples
#' pts <- data.frame(x = rnorm(200, 0, 5))
#' cell_count_1d(pts, xmin = min(pts$x), xmax = max(pts$x), n = 5)
#'
#'

cell_count_1d <- function(pts, xmin, xmax, n) {
  x <- ceiling(rescale(pts$x, xmin, xmax, n))
  x[x==0] <- 1
  cnt <- table(x)
  sum(cnt > 0)
}

rescale <- function(x, xmin, xmax, n) {
  return((x - xmin) / (xmax - xmin) * n)
}
