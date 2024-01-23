#' Calculate fractal dimension using cube counting
#'
#' @param data object of class RasterLayer or mesh3d
#' @param lvec scales to use for calculation (i.e. cube sizes)
#' @param keep_data Keep calculation data. Default = FALSE.
#' @param plot Plot number of filled cubes at different scales. Default = FALSE.
#'
#' @details This function calculates fractal dimension using the cube counting method.
#' Based on lvec, cubes of different sizes are defined and the function counts mesh points that fall within each cube.
#' It is recommended that to specify the maximum value of `lvec` so that the largest box encapsulates the entire object.
#' The smallest scale included in `lvec` should not be smaller than the resolution of your object.
#' Most meshes are not perfectly fractal, and so use the `plot` parameter to look for scale transitions.
#'
#' @return A `data.frame` of number of cubes (`n`) intersecting mesh points at different cube sizes (`L`) and a fractal dimension value. Note that cube size is the length of a side.
#' @export
#' @seealso [fd()]
#' @examples
#' fd_cubes(mcap, lvec = c(0.05, 0.1, 0.25, 0.5))
#'
fd_cubes <- function(data, lvec, plot = FALSE, keep_data = FALSE) {

  if (is(data, "RasterLayer")) {
    pts <- as.data.frame(data, xy = TRUE)
    res <- raster::res(data)[1]
  } else if (is(data, "mesh3d")) {
    pts <- data.frame(t(data$vb)[,1:3])
    res <- max(Rvcg::vcgMeshres(data)[[2]])
  } else {
    stop("data must be of class RasterLayer or mesh3d with triangular mesh")
  }
  names(pts) <- c("x", "y", "z")
  L0 <- min(lvec)
  Lmax <- max(lvec)

  # some checks
  if (min(lvec) < res){
    warning("The smallest scale included in lvec is smaller than recommended.")
  }
  if (max(lvec) < max(diff(apply(pts, 2, range)))){
    warning("The largest scale included in lvec is smaller than recommended. Consider adjusting to a size that encapsulate the entire mesh.")
  }

  x0 <- min(pts[,1]) - res/2
  y0 <- min(pts[,2]) - res/2
  z0 <- min(pts[,3]) - res/2

  cubes <- unique(round(Lmax/lvec))
  l <- unique(Lmax/cubes)

  n <- sapply(cubes, function(n) cell_count(pts, x0, x0 + Lmax, y0, y0 + Lmax, z0, z0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(l))
  f <- -as.numeric(coef(mod)[2])

  # plot
  if (plot) {
    plot(log10(n) ~ log10(l))
    abline(mod)
  }
  # output
  if (keep_data) {
    return(list(fd = f, data = data.frame(l = l, n = n)))
  } else {
    return(f)
  }
}

