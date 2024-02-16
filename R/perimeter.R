#' Calculate perimeter of a 2D shape
#'
#' @description
#' Calculates the perimeter of a 2D shape.
#'
#' @param data A data frame with the first two columns ordered x and y coordinates.
#' @param keep_data Logical. Keep lengths of all segments of the perimeter? Defaults to FALSE.
#'
#' @return The perimeter.
#' @export
#'
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
