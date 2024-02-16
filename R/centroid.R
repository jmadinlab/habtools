#' Calculate the centroid of 3D points
#'
#' @description
#' Calculates the centroid for a given set of XYZ coordinates.
#'
#' @param data A data frame with x, y, and z coordinates.
#'
#' @return The coordinates of the centroid.
#' @export
#'
#' @examples
#' data <- mesh_to_points(mcap)
#' centroid(data)

centroid <- function(data) {
  xmean <- mean(data[,1])
  ymean <- mean(data[,2])
  zmean <- mean(data[,3])
  return(c(xmean, ymean, zmean))
}
