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
    data <- terra::as.data.frame(data, xy = T)

    if (missing(x)) x <- min(data$x)
    if (missing(y)) y <- min(data$y)
    if (missing(L)) L <- max(data$x) - min(data$x)

    sub <- data[data$x > x & data$x<= x + L &  data$y > y & data$y<= y + L, ]
    out <- max(sub[,3], na.rm = TRUE) - min(sub[,3], na.rm = TRUE)
  }
  if (method=="mesh") {
    pts <- data.frame(t(data$vb)[,1:3])
    names(pts) <- c("x", "y", "z")

    if (missing(x)) x <- min(pts$x)
    if (missing(y)) y <- min(pts$y)
    if (missing(L)) L <- min(c(diff(range(pts$x)), diff(range(pts$y))))

    pts <- pts[pts$x >= x & pts$x <= x+L & pts$y >= y & pts$y <= y+L,]
    out <- diff(range(pts$z))
  }
  if (plot) rect(x, y, x + L, y + L)
  return(out)
}
