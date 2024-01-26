#' The perimeter of a 2D shape
#'
#' @description
#' `mesh_to_d2` turns a 3D Mesh file into an xy data frame.
#'
#' @param data A data frame with the first two columns ordered x and y coordinates.
#'
#' @return The perimeter.
#' @export
#'
#' @details
#' The function rasterizes uses the vertices of the mesh file.
#'
#' @examples
#' mcap_2d <- mesh_to_2d(mcap)
#' plot(mcap_2d)
#'
#' perimeter(mcap_2d)
#'
#' r <- 1 # radius
#' circ <- sim_circle(r=r) # simulate xy coordinates for a circle of radius 1
#' plot(circ, asp=1)
#' perimeter(circ)
#'
#' 2 * pi * r # Note xy resolution affects output
#'

perimeter <- function(data, keep_data=FALSE){
  segs <- sapply(1:(nrow(data)-1), function(i) {dist(data[c(i, i+1),])})
  p <- sum(segs)
  if (keep_data) {
    return(list(perimeter=p, segments=segs))
  } else {
    return(p)
  }
}
