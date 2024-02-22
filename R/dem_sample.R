#' Sample a random DEM with specified size from a larger DEM
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param L Size of square to cut out of DEM.
#'
#' @return Digital elevation model of class RasterLayer.
#' @export
#'
#' @examples
#' dem <- dem_sample(horseshoe, L = 4)
#' raster::plot(dem)

dem_sample <- function(data, L, allow_NA=TRUE, plot=FALSE) {

  sub <- dem_runif(data, L)
  while (sub$NAs != allow_NA) {
    sub <- dem_runif(data, L)
  }

  dem_crop(data, x0 = sub$x0, y0 = sub$y0, L = L, plot=plot)

  # size <- L
  # mid <- mid_find(data)
  # sub <- dem_crop(data,
  #                 x0 = mid$x_mid[1],
  #                 y0 = mid$y_mid[1],
  #                 L = raster::extent(data)[2] - raster::extent(data)[1] - size)
  # xy <- raster::rasterToPoints(sub)
  # xyi <- xy[sample(1:nrow(xy), size = 1),]
  #
  # dem_crop(data, x0 = xyi[1], y0 = xyi[2], L = size, plot=plot)
}


dem_runif <- function(data, L) {
  xmin <- raster::extent(data)[1] + L/2
  xmax <- raster::extent(data)[2] - L/2
  ymin <- raster::extent(data)[3] + L/2
  ymax <- raster::extent(data)[4] - L/2

  x0 <- (xmin + L/2) + runif(1) * (xmax - xmin)
  y0 <- (ymin + L/2) + runif(1) * (ymax - ymin)

  sub <- dem_crop(data, x0, y0, L)
  NAs <- any(is.na(raster::values(sub)))

  return(list(sub=sub, NAs=NAs, x0=x0, y0=y0))
}
