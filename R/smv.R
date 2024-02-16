#' Calculate second moment of volume
#'
#' @description
#' Calculates the 2nd moment of volume (SMV) by multiplying the volume of each
#' triangle in the mesh by its centroids' distance from the origin (should be set to the attachment point
#' of the mesh). The sum of these values is the 2nd moment of volume.
#' Axis is z by default, meaning it will calculate the vertical second moment, but this can be changed if needed.
#' This metric is size-dependent so to compare moments in terms of shape only, set scale = TRUE.
#'
#' @param mesh A triangular mesh of class mesh3d.
#' @param axis The axis along which to calculate the second moment of volume z is the default.
#' @param scale Logical. Scale the object to have a volume = 1? Default = TRUE.
#' @param origin Logical. Set the origin to the bottom left corner of bounding box? Default = FALSE
#'
#' @return SMV value.
#' @export
#'
#' @examples
#' smv(mcap)

smv <- function(mesh, axis = "z", scale = FALSE, origin = TRUE){
  if (origin) {
    mesh <- set_origin(mesh)
  }
  if (scale) {
    mesh <- scale_volume(mesh)
  }
  Dim <- ifelse(axis == "z",3,ifelse(axis == "y",2,1)) #set the dimension based on Axis value
  VolMoments <- sapply(1:ncol(mesh$it), function(x) {
    svol_triangle(as.vector(t(mesh$vb[,mesh$it[,x]][-4,]))) * #calculate the signed volume of a triangle
      centroid(rbind(t(matrix(as.vector(mesh$vb[,mesh$it[,x]][-4,]))),c(0,0,0)))[Dim] #multiply it by the distance to the origin along Axis from the centroid of the prism
  })
  return(sum(VolMoments))
}
