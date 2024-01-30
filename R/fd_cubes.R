#' Calculate fractal dimension using cube counting
#'
#' @param data object of class RasterLayer or mesh3d
#' @param lvec scales to use for calculation (i.e. cube sizes)
#' @param keep_data Keep calculation data. Default = FALSE.
#' @param plot Plot number of filled cubes at different scales. Default = FALSE.
#' @param scale Rescale height values to the extent. Only relevant for DEMs. (Defaults to FALSE)
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
#' fd_cubes(mcap, plot=TRUE, keep_data=TRUE)
#' fd_cubes(mcap, lvec = c(0.05, 0.1, 0.25, 0.5), plot=TRUE)
#'
#' fd_cubes(horseshoe, plot=TRUE, keep_data=TRUE)
#'
fd_cubes <- function(data, lvec, plot = FALSE, keep_data = FALSE, scale = FALSE) {

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

  if (missing(lvec)) {
    Lmax <- max(diff(apply(pts, 2, range))) + res
    L0 <- res
    lvec <- 2^seq(log2(L0), log2(Lmax), length.out=10)
  } else {
    L0 <- min(lvec)
    Lmax <- max(lvec)
  }

  # some checks
  if (min(lvec) < res){
    warning("The smallest scale included in lvec is smaller than recommended.")
  }
  if (max(lvec) < Lmax){
    warning("The largest scale included in lvec is smaller than recommended. Consider adjusting to a size that encapsulate the entire mesh.")
  }

  if (scale) {
    data[] <-((data[] - min(data[]))/(max(data[]) - min(data[])) ) * L
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
    if (is(data, "RasterLayer")) {
      plot(data, axes=FALSE)
    } else {
      plot(pts[,1:2], asp=1, type="l", axes=FALSE, xlim=c(x0, x0 + Lmax), ylim=c(y0, y0 + Lmax))
    }
    rect(x0, y0, x0 + l, y0 + l, border="red")
    axis(1)
    axis(2, las=2)
  }
  # output
  if (keep_data) {
    return(list(fd = f, lvec=l, data = data.frame(l = l, n = n)))
  } else {
    return(f)
  }
}

