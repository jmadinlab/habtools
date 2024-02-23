#' Sample a random DEM with specified size from a larger DEM
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param L Size of square to cut out of DEM.
#' @param allow_NA Logical. Allow NAs in the sample? Useful when DEM is not regular.
#' @param plot Logical. Plot the DEM and the cropped section?
#' @param max_iter Maximum number of random crops to try when allow_NA = FALSE before failing.
#'
#' @note Not allowing NAs may increase sampling time for irregular DEMs that contain a lot of NAs; e.g., structure from motion transects.
#'
#' @return Digital elevation model of class RasterLayer.
#' @export
#'
#' @importFrom stats runif
#'
#' @examples
#' dem <- dem_sample(horseshoe, L = 2, plot=TRUE)
#'

dem_sample <- function(data, L, allow_NA=TRUE, plot=FALSE, max_iter=100) {
  iter <- 1
  sub <- dem_runif(data, L)
  while (sub$NAs & (!allow_NA)) {
    sub <- dem_runif(data, L)
    iter <- iter + 1
    if (iter == max_iter) {
      stop("Maximum iterations reached. Double check that there is actually an area that can be sampled given L. If so, you can increase max_iter, but expect the function to take longer to run.")
    }
  }
  dem_crop(data, x0 = sub$x0, y0 = sub$y0, L = L, plot=plot)
}


dem_runif <- function(data, L) {
  xmin <- raster::extent(data)[1] + L/2
  xmax <- raster::extent(data)[2] - L/2
  ymin <- raster::extent(data)[3] + L/2
  ymax <- raster::extent(data)[4] - L/2

  x0 <- runif(1, xmin, xmax)
  y0 <- runif(1, ymin, ymax)

  sub <- dem_crop(data, x0, y0, L)
  NAs <- any(is.na(raster::values(sub)))

  return(list(sub=sub, NAs=NAs, x0=x0, y0=y0))
}
