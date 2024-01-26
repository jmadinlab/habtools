#' Circularity of 2D shape
#'
#' @description
#' Calculated the `circularity` of a 2D shape.
#'
#' @param data A data frame with the first two columns x and y coordinates, respectively.
#'
#' @return A value between 0 (infinitely irregular) and 1 (a perfect circle).
#' @export
#'
#' @details
#' The perimeter of the 2D shape is divided by the perimeter of a circle with
#' the same area as the shape. The more irregular the shape is, the closer the
#' output value is to zero. The closer the shape is to a circle, the closer
#' the output value is to 1.
#'
#' @examples
#' mcap_2d <- mesh_to_2d(mcap)
#' circularity(mcap_2d)
#'
#' circ <- sim_circle() # simulate xy coordinates for a circle
#' circularity(circ)
#'

circularity <- function(data) {
  (2 * pi * sqrt(geometry::polyarea(data[,1], data[,2])/pi)) / perimeter(data)
}

circularity_2 <- function(data){ # This is the square of what I think it should be, but seems to be used widely.
  4*pi*geometry::polyarea(data[,1], data[,2]) / (perimeter(data)^2)
}
