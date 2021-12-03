#' Height range
#'
#' @param data A three-dimensional object.
#' @param method `dem` (default) is digital elevation model (DEM) in raster format; `mesh` is a 3D mesh model with vertices.
#'
#' @return The difference between the lowest and highest point in a 3D object.
#'
#' @export
#'
#' @examples
#' hr(horseshoe)
#'
#' plot(horseshoe)
#'
#' # Within a bounding box
#' x <- -470
#' y <- 1266
#' L <- 2
#' rect(x, y, x+L, y+L)
#'
#' ext <- extent(x, x+L, y, y+L)
#' hr(crop(horseshoe, ext))
#'
#' # For a 3D mesh.
#' hr(mcap, method="mesh")
#'
hr <- function(data, method="dem") {
  if (method=="dem") {
    hr <- diff(range(raster::getValues(data), na.rm=TRUE))
  }
  if (method=="mesh") {
    z <- (data$vb)[3,]
    hr <- diff(range(z))
  }
  return(hr)
}
