#' Second moment area
#'
#' Calculates the 2nd moment of surface area about the origin by multiplying the surface area of
#' each triangle in the mesh by it's distance from the origin (should be set to
#' the attachment point of the mesh). The sum of these values is the 2nd moment
#' of area.
#'
#' @param mesh A mesh object
#' @param axis the axis along which to calculate the second moment of area. z is the default.
#' @param origin set the origin to the bottom left corner of bounding box
#' @param scale scale object so that total area = 1
#' @return value
#' @export
#'
#' @examples
#' sma(mcap)
#' sma(mcap, scale = T)

sma <- function(mesh, axis = "z", scale = TRUE, origin = TRUE){
  if (origin) {
    mesh <- set_origin(mesh)
  }
  if (scale) {
    mesh <- scale_area(mesh)
  }
  Dim <- ifelse(axis == "z",3,ifelse(axis == "y",2,1)) #set the dimension based on Axis value
  SAMoments <- suppressWarnings(sapply(1:ncol(mesh$it), function(x) {
    sa_triangle(as.vector(t(mesh$vb[,mesh$it[,x]][-4,]))) * #calculate the surface area
      centroid(matrix(as.vector(t(mesh$vb[,mesh$it[,x]][-4,])),3,3))[Dim] #multiply it by the distance to the origin along Axis
  }))
  return(sum(SAMoments))
}

