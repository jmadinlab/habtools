#' Calculate fractal dimension using the surface area method
#'
#' @param data dem of class "RasterLayer" or mesh class "mesh3d"
#' @param lvec vector of scales to include in calculation
#' @param keep_data Keep area data. Default = FALSE.
#' @param plot Plot regression line and area data. Default = FALSE.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#'
#' @return Either a value or a list
#' @export
#'
#' @examples
#' fd_area(mcap, lvec = c(0.01, 0.02, 0.04, 0.08, 0.16))
#' fd_area(horseshoe, lvec = c(0.1, 0.2, 0.4, 0.8, 1.6))
#'
fd_area <- function(data, lvec, x, y, L, keep_data = FALSE, plot = FALSE) {
    if (is(data, "RasterLayer")) {

      if (sum(is.na(values(data))) > 0) {
        message(paste0("data contains ", sum(is.na(values(data))), " NA values. Results may be biased."))
      }


      if (missing(x)) x <- raster::xmin(data)
      if (missing(y)) y <- raster::ymin(data)
      if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))

      if (L < min(dim(data)[1:2] * raster::res(data))) {
        b <- as(raster::extent(x, x + L, y, y + L), 'SpatialPolygons')
        raster::crs(b) <- raster::crs(data)
        data <- raster::crop(data, b)
      }

      if(res(data)[1] - (min(lvec)) > 0.00001) {
        stop("Values in lvec need to be equal to or larger than the resolution of data")
      }
    a <- sapply(lvec, function(l){
      fac <- round(l/raster::res(data)[1])
      r <- raster::aggregate(data, fac, fun = "mean")
      g <- as(r, 'SpatialGridDataFrame')
      sa <- sp::surfaceArea(g, byCell = TRUE)
      sum(raster::values(raster::raster(sa)))/(raster::extent(sa)[2] - raster::extent(sa)[1])^2
      })
  } else if (is(data, "mesh3d")) {
    if(min(lvec) < Rvcg::vcgMeshres(data)[1]) {
      stop("Values in lvec need to be equal to or larger than the resolution of data")
    }
    a <- sapply(lvec, function(l){
      mesh <- Rvcg::vcgQEdecim(data, edgeLength = l, silent = T)
      Rvcg::vcgArea(mesh)
      })
  } else {
    stop("data must be of class RasterLayer or mesh3d")
  }
  f <- 2 - coef(lm(log10(a) ~ log10(lvec)))[2]
  df <- data.frame(l = lvec, area = a)

  if (plot) {
    plot(log10(a) ~ log10(lvec))
    pred <- predict(lm(log10(a) ~ log10(lvec)))
    lines(log10(lvec), pred, lty = 1)
  }

  if (keep_data) {
    return(list(fd = unname(f), data = df))
  } else {
    return(unname(f))
  }
}

