#' Second moment volume
#'
#' Calculates the 2nd moment of volume by multiplying the volume of each
#' triangle in the mesh by its centroids' distance from the origin (should be set to the attachment point
#' of the mesh). The sum of these values is the 2nd moment of volume.
#' To compare moments in terms of shape only, set scale = TRUE.
#' Axis is z by default, meaning it will calculate the vertical second moment, but this can be changed if needed.
#'
#' @param mesh A 3D mesh object.
#' @param axis default = z.
#' @param scale To scale the object to have a volume = 1.
#' @param origin set the origin to the bottom left corner of bounding box
#'
#' @return second moment of volume
#' @export
#'
#' @examples
#' smv(mcap)

smv <- function(mesh, axis = "z", scale = TRUE, origin = TRUE){
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
