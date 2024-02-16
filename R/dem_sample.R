#' Sample a random DEM with specified size from a larger DEM
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param L Size of square to cut out of DEM.
#'
#' @return Digital elevation model of class RasterLayer.
#' @export
#'
#' @examples
#' dem1 <- sample_dem(horseshoe, L = 4)
#' raster::plot(dem1)
sample_dem <- function(data, L) {
  size <- L
  mid <- mid_find(data)
  sub <- dem_crop(data,
                  x0 = mid$x_mid[1],
                  y0 = mid$y_mid[1],
                  L = raster::extent(data)[2] - raster::extent(data)[1] - size)
  xy <- raster::rasterToPoints(sub)
  xyi <- xy[sample(1:nrow(xy), size = 1),]

  dem_crop(data, x0 = xyi[1], y0 = xyi[2], L = size)
}
