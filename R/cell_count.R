#' Count filled cells
#'
#' @param pts data
#' @param xmin minimum x-value
#' @param xmax maximum x-value
#' @param ymin minimum y-value
#' @param ymax maximum y-value
#' @param zmin minimum z-value
#' @param zmax maximum z-value
#' @param n multiplier
#'
#' @return
#' @export
#'
cell_count <- function(pts, xmin, xmax, ymin, ymax, zmin, zmax, n) {
  cnt <- table(ceiling(rescale(pts$x, xmin, xmax, n)),
               ceiling(rescale(pts$y, ymin, ymax, n)),
               ceiling(rescale(pts$z, zmin, zmax, n)))
  sum(cnt > 0)
}
