#' Rescale
#'
#' @param x
#' @param xmin
#' @param xmax
#' @param n
#'
#' @return
#' @export
#'
#' @examples
rescale <- function(x, xmin, xmax, n) {
  return((x - xmin) / (xmax - xmin) * n)
}
