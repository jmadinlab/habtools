#' Rescale
#'
#' @param x vector
#' @param xmin minimum value
#' @param xmax maximum value
#' @param n multiplier
#'
#' @return
#' @export
#'
rescale <- function(x, xmin, xmax, n) {
  return((x - xmin) / (xmax - xmin) * n)
}
