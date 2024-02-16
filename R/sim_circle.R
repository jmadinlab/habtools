#' Simulate a circle
#'
#' @description
#' Simulates xy coordinates for a circle of given radius. Created for package
#' testing purposes, but might be useful for others.
#'
#' @param r Radius of the circle (default 1).
#' @param n Number of xy coordinates defining the circle (default 100).
#' @param mid Mid point of the circle (default 0, 0).
#'
#' @return A data frame of n xy-coordinates.
#' @export
#'
#' @examples
#' circ <- sim_circle()
#' plot(circ)
#'
#' circularity(circ)
#' perimeter(circ)
#'

sim_circle <- function(r=1, n=100, mid = c(0, 0)) {
  p <- seq(0, 2 * pi, length.out = n)
  data.frame(x = mid[1] + (r * cos(p)), y = mid[2] + (r * sin(p)))
}
