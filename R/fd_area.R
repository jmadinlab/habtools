#' Calculate fractal dimension using the surface area method
#'
#' @param data DEM of class "RasterLayer" or mesh of class "mesh3d".
#' @param lvec Vector of scales to include in calculation.
#' @param keep_data Logical. Keep data? Default is FALSE.
#' @param plot Logical. Plot regression line and area data?  Defaults to FALSE.
#' @param scale Logical. Rescale height values to fit the extent? Only relevant for DEMs. Defaults to FALSE.
#'
#' @return Either a value or a list if keep_data = TRUE.
#' @export
#'
#' @examples
#' fd_area(mcap, lvec = c(0.01, 0.02, 0.04, 0.08, 0.16))
#' fd_area(horseshoe, lvec = c(0.06125, 0.125, 0.25, 0.5, 1))
#'
#' fd_area(mcap)
#' fd_area(horseshoe)
#'
fd_area <- function(data, lvec = NULL, keep_data = FALSE, plot = FALSE, scale = FALSE) {
    if (is(data, "RasterLayer")) {
      if (sum(is.na(values(data))) > 0) {
        message(paste0("Data contains ", sum(is.na(values(data))), " NA values. Results may be biased."))
      }

      L0 <- min(raster::res(data))
      L <- extent(data)

      if (missing(lvec)) {
        lvec <- L / 2^(0:20)
        lvec <- sort(lvec[lvec >= L0*2 & lvec <= L/4])
        print(paste0("lvec is set to c(", toString(round(lvec, 3)), ")."))
      } else {
        lvec <- sort(lvec)
      }

      if (raster::res(data)[1] - (min(lvec)) > 0.00001) {
        stop("Values in lvec need to be equal to or larger than the resolution of data")
      }

      if (scale) {
        data[] <-((data[] - min(data[]))/(max(data[]) - min(data[])) ) * L
      }

    a <- sapply(lvec, function(l){
      bb <- raster::bbox(data)
      temp <- raster::raster(xmn=bb[1,1], xmx=bb[1,2],
                             ymn=bb[2,1], ymx=bb[2,2], resolution = l,
                             crs = raster::crs(data))
      r <- terra::project(terra::rast(data), terra::rast(temp))
      r <- raster::raster(r)
      g <- as(r, 'SpatialGridDataFrame')
      sa <- sp::surfaceArea(g, byCell = TRUE)
      # relative area to account for variations in planar area with varying scales in case L/lvec are not whole numbers
      sum((raster::values(raster::raster(sa)))*L^2)/(habtools::extent(r))^2
      })

  } else if (is(data, "mesh3d")) {

    L <- extent(data)
    L0 <- median(Rvcg::vcgMeshres(data)[[2]])

    if (missing(lvec)) {
      lvec <- 2^seq(log2(L0),log2(L/20), length.out = 5)
      print(paste0("lvec is set to c(", toString(round(lvec, 3)), ")."))
    }

    if(min(lvec) < L0) {
      stop("Values in lvec need to be equal to or larger than the resolution of the object")
    }
    a <- sapply(lvec, function(l){
      if (l == L0) {
        mesh <- data
      } else {
        mesh <- Rvcg::vcgQEdecim(data, edgeLength = l, silent = T, scaleindi = F)
      }
      Rvcg::vcgArea(mesh)
      })
  } else {
    stop("data must be of class RasterLayer or mesh3d")
  }
  f <- 2 - coef(lm(log10(a) ~ log10(lvec)))[2]
  df <- data.frame(l = lvec, area = a)

  # if (plot) {
  #   plot(log10(a) ~ log10(lvec))
  #   pred <- predict(lm(log10(a) ~ log10(lvec)))
  #   lines(log10(lvec), pred, lty = 1)
  #
  # }
  # plot
  if (plot) {
    if (is(data, "RasterLayer")) {
      plot(data, axes=FALSE)
      x0 <- raster::extent(data)[1]
      y0 <- raster::extent(data)[3]
      rect(x0, y0, x0 + lvec, y0 + lvec, border="red")
      axis(1)
      axis(2, las=2)
    } else {
      plot(mesh_to_2d(data), asp=1, type="l", axes=FALSE)
      x0 <- min(data$vb[1,])
      y0 <- min(data$vb[2,])
      rect(x0, y0, x0 + lvec, y0 + lvec, border="red")
      axis(1)
      axis(2, las=2)
    }

  }
  if (keep_data) {
    return(list(D = unname(f), lvec = lvec, data = df, method = "area"))
  } else {
    return(unname(f))
  }
}


