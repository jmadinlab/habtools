#' Crop DEM around Points
#'
#' @param data a DEM in RasterLayer format
#' @param x0 A value or vector of center x coordinate(s)
#' @param y0 A value or vector of center y coordinate(s)
#' @param L Size of square to cut out of DEM
#' @param plot Plot the DEM and the cropped sections (TRUE or FALSE)
#'
#' @return A cropped RasterLayer or a list of RasterLayers
#' @export
#'
#' @examples
#' # around one point
#' dem_cropped <- crop_dem(horseshoe, -468, 1266, L = 2)
#' raster::plot(dem_cropped[[1]])
#'
#' # around multiple points
#' points <- data.frame(x = c(-467, -465, -466), y = c(1270, 1265, 1268))
#' dem_list <- crop_dem(horseshoe, points$x, points$y, L = 1, plot = T)

crop_dem <- function(data, x0, y0, L, plot = FALSE) {
  if (!length(x0) == length(y0)) {
    stop("x0 and y0 need to have the same length")
  }
  if (plot) {
    raster::plot(data)
    rect(x0 - (L/2), y0 - (L/2), x0 + (L/2), y0 + (L/2))
  }
  if (length(x0) == 1) {
    out <- raster::crop(data, raster::extent(x0, y0, x0 + L, y0 + L))
  } else if (length(x0) > 1) {
    out <-
      lapply(1:length(x0), function(i) {
        raster::crop(data, raster::extent(x0[i], y0[i], x0[i] + L, y0[i] + L))
      })
  }

  return(out)

}
