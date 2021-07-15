#' Count filled cells
#'
#' @param pts
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @param zmin
#' @param zmax
#' @param n
#'
#' @return
#' @export
#'
#' @examples
cell_count <- function(pts, xmin, xmax, ymin, ymax, zmin, zmax, n) {
  cnt <- table(ceiling(rescale(pts$x, xmin, xmax, n)),
               ceiling(rescale(pts$y, ymin, ymax, n)),
               ceiling(rescale(pts$z, zmin, zmax, n)))
  sum(cnt > 0)
}
