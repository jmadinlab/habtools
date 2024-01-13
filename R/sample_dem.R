#' Sample a random DEM with specified size from a larger DEM
#'
#' @param data A DEM in RasterLayer format
#' @param L Size of square to cut out of DEM
#'
#' @return A DEM in RasterLayer format
#' @export
#'
#' @examples
#' sample_dem(horseshoe, L = 4)
sample_dem <- function(data, L) {
  size <- L
  mid <- mid_find(data)
  sub <- crop_dem(data,
                  x0 = mid$x_mid[1],
                  y0 = mid$y_mid[1],
                  L = raster::extent(data)[2] - raster::extent(data)[1] - size)
  xy <- raster::rasterToPoints(sub)
  xyi <- xy[sample(1:nrow(xy), size = 1),]

  crop_dem(data, x0 = xyi[1], y0 = xyi[2], L = size)
}
