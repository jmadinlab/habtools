#' Height Variation in cells at different scales
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param lvec scales to use for calculation
#' @param parallel TRUE or FALSE. Use parallel processing? Note: parallel must be installed.
#' @param ncores number of cores to use when parallel = TRUE.
#'
#' @return A `data.frame` containing height ranges of cells at different scales.
#' @export
#'
#' @details
#' This function is used for calculating fractal dimension using the
#' height variation method.
#'
#' @examples
#'
#' hvar(horseshoe, x = -470, y = 1266, L = 2, lvec = c(0.125, 0.25, 0.5, 1))

hvar <- function(data, x, y, lvec, L,
                 parallel = FALSE,
                 ncores = (parallel::detectCores()-1)) {


  if (sum(is.na(values(data))) > 0) {
    message(paste0("data contains ", sum(is.na(values(test))), " NA values. Results may be biased."))
  }


  if (missing(x)) x <- raster::xmin(data)
  if (missing(y)) y <- raster::ymin(data)
  if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))

  if (L < min(dim(data)[1:2] * raster::res(data))) {
    b <- as(raster::extent(x, x + L, y, y + L), 'SpatialPolygons')
    raster::crs(b) <- raster::crs(data)
    data <- raster::crop(data, b)
  }
  hvar <-
    lapply(lvec, function(l){
      list <- split_dem(data, l, parallel = parallel, ncores = ncores)
      h <- sapply(list, hr)
      data.frame(l = l, h = h)
    }) %>% dplyr::bind_rows()

  return(hvar)
}
