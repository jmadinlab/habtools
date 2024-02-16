#' Crop DEM around points
#'
#' @description
#' A function for sampling a DEM by cropping squares of a given size around xy coordinates.
#'
#' @param data A DEM in RasterLayer format.
#' @param x0 A value or vector of central x coordinate(s).
#' @param y0 A value or vector of central y coordinate(s).
#' @param L Size of squares to cropped from the DEM.
#' @param plot Logical. Plot the DEM and the cropped sections?
#'
#' @return A cropped RasterLayer or list of RasterLayers.
#' @export
#'
#' @examples
#' # around one point
#' dem_cropped <- dem_crop(horseshoe, -468, 1266, L = 2)
#' raster::plot(dem_cropped)
#' points(-468, 1266)
#'
#' # around multiple points
#' points <- data.frame(x = c(-467, -465, -466), y = c(1270, 1265, 1268))
#' dem_list <- dem_crop(horseshoe, points$x, points$y, L = 1, plot = TRUE)
#'
#' # plot the first element
#' raster::plot(dem_list[[1]])

dem_crop <- function(data, x0, y0, L, plot = FALSE) {
  if (!length(x0) == length(y0)) {
    stop("x0 and y0 need to have the same length")
  }
  if (plot) {
    raster::plot(data, asp=1)
    rect(x0 - (L/2), y0 - (L/2), x0 + (L/2), y0 + (L/2))
  }
  if (length(x0) == 1) {
    out <- raster::crop(data, raster::extent(x0 - (L/2),  x0 + (L/2), y0 - (L/2), y0 + (L/2)))
  } else if (length(x0) > 1) {
    out <-
      lapply(1:length(x0), function(i) {
        raster::crop(data, raster::extent(x0[i] - (L/2), x0[i] + (L/2), y0[i] - (L/2), y0[i] + (L/2)))
      })
  }
  return(out)
}
