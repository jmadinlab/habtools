#signedVolumeOfTriangle ====
#Calculates the signed volume of a triangle based on a set of XYZCoords
#Signed volume means that volumes can take on a negative value depending on whether the surface normal
#of the triangle is facing towards or away from the origin. When all positive and negative volumes
#are integrated across the entire mesh, they values cancel out so that the final volume is an approximation
#of the total volume of the mesh.
#' Signed volume of triangle
#'
#' Calculates the signed volume of a triangle based on a set of XYZCoords.
#' Signed volume means that volumes can take on a negative value depending on whether the surface normal
#' of the triangle is facing towards or away from the origin. When all positive and negative volumes
#' are integrated across the entire mesh, these values cancel out so that the final volume is an approximation
#' of the total volume of the mesh.
#'
#' @param XYZcoords A dataframe with XYZ coordinates of three points in following order: X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
#'
#' @return the signed volume of the triangle
#' @export
#'
#' @examples
#' svol_triangle(c(X1 = 1, X2 = 2, X3 = 3 , Y1 = 1, Y2 = 2, Y3 = 1, Z1 = 1, Z2 = 1, Z3 = 1))
svol_triangle <- function(XYZCoords) {
  v321 <- XYZCoords[3] * XYZCoords[5] * XYZCoords[7]
  v231 <- XYZCoords[2] * XYZCoords[6] * XYZCoords[7]
  v312 <- XYZCoords[3] * XYZCoords[4] * XYZCoords[8]
  v132 <- XYZCoords[1] * XYZCoords[6] * XYZCoords[8]
  v213 <- XYZCoords[2] * XYZCoords[4] * XYZCoords[9]
  v123 <- XYZCoords[1] * XYZCoords[5] * XYZCoords[9]
  (1.0/6.0)*(-v321 + v231 + v312 - v132 - v213 + v123);
}
