#' Height range
#'
#' @param data A digital elevation model (DEM) in a "RasterLayer" class or a "mesh3d" object.
#' @param x Only used for RasterLayers. The lower-left x-coordinate of bounding box. Default is the minimum x coordinate.
#' @param y The lower-left y-coordinate of bounding box. Default is the minimum y coordinate.
#' @param L The bounding box size (i.e., side length). Default is the maximum.
#'
#' @return The difference between the lowest and highest point.
#'
#' @export
#'
#' @examples
#'
#' library(raster)
#' plot(horseshoe)
#' hr(horseshoe)
#' hr(horseshoe, -470, 1266, 2)
#'
#' # For a 3D mesh.
#' hr(mcap)
#'
hr <- function(data, x, y, L) {


  if (is(data, "RasterLayer")) {
    data <- terra::as.data.frame(data, xy = T)

    if (missing(x)) x <- min(data$x)
    if (missing(y)) y <- min(data$y)
    if (missing(L)) L <- max(data$x) - min(data$x)

    sub <- data[data$x > x & data$x<= x + L &  data$y > y & data$y<= y + L, ]

    out <- NA
    if (!all(is.na(sub[,3]))) {
      out <- max(sub[, 3], na.rm = TRUE) - min(sub[, 3], na.rm = TRUE)
    }
  } else if (is(data, "data.frame")) {
    if (missing(x)) x <- min(data$x)
    if (missing(y)) y <- min(data$y)
    if (missing(L)) L <- max(data$x) - min(data$x)

    sub <- data[data$x > x & data$x<= x + L &  data$y > y & data$y<= y + L, ]

    out <- NA
    if (!all(is.na(sub[,3]))) {
      out <- max(sub[, 3], na.rm = TRUE) - min(sub[, 3], na.rm = TRUE)
    }
  } else if (is(data, "mesh3d")) {
    pts <- data.frame(t(data$vb)[,1:3])
    names(pts) <- c("x", "y", "z")

    if (missing(x)) x <- min(pts$x)
    if (missing(y)) y <- min(pts$y)
    if (missing(L)) L <- min(c(diff(range(pts$x)), diff(range(pts$y))))

    pts <- pts[pts$x >= x & pts$x <= x+L & pts$y >= y & pts$y <= y+L,]
    out <- diff(range(pts$z))
  } else {
    stop("data must be either an object of class RasterLayer, data.frame, or mesh3d")
  }
  return(out)
}
