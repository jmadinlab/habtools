#' Count filled cells
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
  cnt <- table(ceiling(rescale(pts$x, xmin, xmax, n)),
               ceiling(rescale(pts$y, ymin, ymax, n)),
               ceiling(rescale(pts$z, zmin, zmax, n)))
  sum(cnt > 0)
}

#' Count filled cells in 2D
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
  cnt <- table(ceiling(rescale(pts$x, xmin, xmax, n)),
               ceiling(rescale(pts$y, ymin, ymax, n)))
  sum(cnt > 0)
}

#' Count filled cells in 1D
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
#' pts <- data.frame(x = rnorm(100, 0, 5))
#' cell_count_1d(pts, xmin = min(pts$x), xmax = max(pts$x), n = 5)
#'
#'

cell_count_1d <- function(pts, xmin, xmax, n) {
  cnt <- table(ceiling(rescale(pts$x, xmin, xmax, n)))
  sum(cnt > 0)
}

rescale <- function(x, xmin, xmax, n) {
  return((x - xmin) / (xmax - xmin) * n)
}
