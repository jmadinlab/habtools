#' Centroid
#'
#' Calculates the centroid for a given set of XYZ coordinates.
#'
#' @param XYZCoords A data frame with x, y, and z coordinates
#'
#' @return the coordinates of the centroid
#' @export
#'
#' @examples
#' data <- mesh_to_points(mcap)
#' centroid(data)
centroid <- function(XYZCoords) {
  xmean <- mean(XYZCoords[,1])
  ymean <- mean(XYZCoords[,2])
  zmean <- mean(XYZCoords[,3])
  return(c(xmean,ymean,zmean))
}
