#' Calculate surface area of triangle
#'
#' Calculates the surface area of a triangle based on a set of XYZCoords.
#'
#' @param XYZcoords A data frame with XYZ coordinates of three points in following order: X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3.
#'
#' @return The surface area of the triangle.
#' @export
#'
#' @examples
#' sa_triangle(c(X1 = 1, X2 = 2, X3 = 3 , Y1 = 1, Y2 = 2, Y3 = 1, Z1 = 1, Z2 = 1, Z3 = 1))
sa_triangle <- function(XYZcoords) {
  ax <- XYZcoords[2] - XYZcoords[1]
  ay <- XYZcoords[5] - XYZcoords[4]
  az <- XYZcoords[8] - XYZcoords[7]
  bx <- XYZcoords[3] - XYZcoords[1]
  by <- XYZcoords[6] - XYZcoords[4]
  bz <- XYZcoords[9] - XYZcoords[7]
  cx <- ay * bz - az * by
  cy <- az * bx - ax * bz
  cz <- ax * by - ay * bx

  0.5 * sqrt(cx*cx + cy*cy + cz*cz)
}
