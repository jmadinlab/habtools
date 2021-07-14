#' Calculate height variation
#'
#' @param x
#' @param y
#' @param s
#'
#' @return
#' @export
#'
#' @examples
hvar <- function(x, y, s) {
  bx <- extent(cbind(c(x0 + x, y0 + y), c(x0 + x + s, y0 + y + s)))
  temp <- diff(range(getValues(raster::crop(data, bx)), na.rm=TRUE))
  return(temp)
}
