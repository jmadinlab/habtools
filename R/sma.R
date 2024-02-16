#' Calculate second moment of area
#'
#' @description
#' Calculates the 2nd moment of surface area about the origin by multiplying the surface area of
#' each triangle in the mesh by its distance from the origin (should be set to
#' the attachment point of the mesh). The sum of these values is the 2nd moment
#' of area.#' This metric is size-dependent so to compare moments in terms of shape only, set scale = TRUE.
#'
#' @param mesh A triangular mesh of class mesh3d.
#' @param axis The axis along which to calculate the second moment of area. z is the default.
#' @param origin Logical. Set the origin to the bottom left corner of bounding box? Default = TRUE.
#' @param scale Logical. Scale the object to have a volume = 1? Default = FALSE
#' @return SMA value.
#' @export
#'
#' @examples
#' sma(mcap)
#' sma(mcap, scale = TRUE)

sma <- function(mesh, axis = "z", scale = FALSE, origin = TRUE){
  if (origin) {
    mesh <- set_origin(mesh)
  }
  if (scale) {
    mesh <- scale_volume(mesh)
  }
  Dim <- ifelse(axis == "z",3,ifelse(axis == "y",2,1)) #set the dimension based on Axis value
  SAMoments <- suppressWarnings(sapply(1:ncol(mesh$it), function(x) {
    sa_triangle(as.vector(t(mesh$vb[,mesh$it[,x]][-4,]))) * #calculate the surface area
      centroid(matrix(as.vector(t(mesh$vb[,mesh$it[,x]][-4,])),3,3))[Dim] #multiply it by the distance to the origin along Axis
  }))
  return(sum(SAMoments))
}

