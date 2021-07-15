#' Count filled cells
#'
#' @param data
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @param zmin
#' @param zmax
#' @param ngrid
#'
#' @return
#' @export
#'
#' @examples
cell_count <- function(data, xmin, xmax, ymin, ymax, zmin, zmax, ngrid) {
  cnt <- table(ceiling(rescale(data[,1], xmin, xmax, ngrid)),
               ceiling(rescale(data[,2], ymin, ymax, ngrid)),
               ceiling(rescale(data[,3], zmin, zmax, ngrid)))
  sum(cnt > 0)
}
