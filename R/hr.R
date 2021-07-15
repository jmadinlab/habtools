#' Height range
#'
#' @param data A digital elevation model (DEM) in raster format.
#' @param x The lower-left x-coordinate of bounding box.
#' @param y The lower-left y-coordinate of bounding box.
#' @param L The bounding box size (i.e., side length).
#' @param method `dem` (default) is digital elevation model (DEM) in raster format; `mesh` is a 3D mesh model with vertices.
#' @param plot Add bounding box to existing plot of `dem`.
#'
#' @return The difference between the lowest and highest point in the
#' bounding box.
#'
#' @export
#'
#' @examples
#' hr(horseshoe)
#'
#' plot(horseshoe)
#' hr(horseshoe, plot=TRUE)
#' hr(horseshoe, -470, 1266, 2, plot=TRUE)
#'
#' # For a 3D mesh.
#' hr(mcap, method="mesh")
#'
hr <- function(data, x, y, L, method="dem", plot=FALSE) {
  if (method=="dem") {
    if (missing(x)) x <- raster::xmin(data)
    if (missing(y)) y <- raster::ymin(data)
    if (missing(L)) L <- dim(data)[1:2] * raster::res(data)

    ext <- raster::extent(cbind(c(x, y), c(x + L, y + L)))
    hr <- diff(range(raster::getValues(raster::crop(data, ext)), na.rm=TRUE))
  }
  if (method=="mesh") {
    pts <- data.frame(t(data$vb)[,1:3])
    names(pts) <- c("x", "y", "z")

    if (missing(x)) x <- min(pts$x)
    if (missing(y)) y <- min(pts$y)
    if (missing(L)) L <- min(c(diff(range(pts$x)), diff(range(pts$y))))

    pts <- pts[pts$x >= x & pts$x <= x+L & pts$y >= y & pts$y <= y+L,]
    hr <- diff(range(pts$z))
  }
  if (plot) rect(x, y, x + L, y + L)
  return(hr)
}
