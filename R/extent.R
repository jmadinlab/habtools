#' Calculate extent of a 3D object
#'
#' @param data An object of class mesh3d
#'
#' @details This function calculates the extent or largest length of the bounding box of a mesh
#'
#' @return A value, the extent of the mesh
#' @export
#' @examples
#' habtools::extent(mcap)
#'
extent <- function(data){
  dt <- t(data$vb)
  max(c(
    abs(max(dt[,1]) - min(dt[,1])),
    abs(max(dt[,2]) - min(dt[,2])),
    abs(max(dt[,3]) - min(dt[,3])))
  )
}
