#' Calculate fractal dimension using the cube counting method
#'
#' @param data An object of class RasterLayer or mesh3d.
#' @param lvec Vector of scales to use for calculation (i.e. cube sizes).
#' @param keep_data Logical. Keep calculation data? Default = FALSE.
#' @param plot Planar representation of cubes superimposed on 3D mesh or DEM for visualizing `lvec`. Default = FALSE.
#' @param scale Logical. Rescale height values to the extent? Only relevant for RasterLayer objects. (Defaults to FALSE).
#'
#' @details This function calculates fractal dimension using the cube counting method.
#' If `lvec` is not specified, a default based on resolution and extent will be used.
#' Based on lvec, cubes of different sizes are defined and the function counts mesh points that fall within each cube.
#' It is recommended to specify the maximum value of `lvec` so that the largest box encapsulates the entire object.
#' The smallest scale included in `lvec` should not be smaller than the resolution of your object.
#'
#' @return A value for fractal dimension, typically between 2 and 3 or a list if keep_data = TRUE.
#' @export
#' @seealso [fd()]
#' @examples
#' fd_cubes(mcap, keep_data = TRUE, plot = TRUE)
#' fd_cubes(mcap, lvec = c(0.05, 0.1, 0.25, 0.5), plot = TRUE)
#'
#' dem <- dem_crop(horseshoe, x0 = -469, y0 = 1267, L = 2, plot = TRUE)
#' fd_cubes(dem, plot = TRUE, keep_data = TRUE)
#' fd_cubes(dem, plot = TRUE, keep_data = TRUE, scale = TRUE)
#'
fd_cubes <- function(data, lvec = NULL, plot = FALSE, keep_data = FALSE, scale = FALSE) {

  if (is(data, "RasterLayer")) {
    if (scale) {
      data[] <-((data[] - min(data[]))/(max(data[]) - min(data[])) ) * extent(data)
    }
    pts <- as.data.frame(data, xy = TRUE)
    res <- raster::res(data)[1]
  } else if (is(data, "mesh3d")) {
    pts <- data.frame(t(data$vb)[,1:3])
    res <- quantile(Rvcg::vcgMeshres(data)[[2]], 0.75)
  } else if (is(data, "data.frame") & ncol(data) == 3){
    pts <- data
    res <- min(lvec)
  } else {
    stop("data must be of class RasterLayer or mesh3d with triangular mesh or data.frame with 3 colums.")
  }

  names(pts) <- c("x", "y", "z")

  if (missing(lvec)) {
    Lmax <- max(diff(apply(pts, 2, range))) + res
    L0 <- res

    cubes <- 2^(0:20)
    lvec <- Lmax / cubes
    cubes <- cubes[lvec > L0]
    lvec <- lvec[lvec > L0]
    message(paste0("lvec is set to c(", toString(sort(round(lvec, 3))), ")."))
  } else {
    L0 <- min(lvec)
    Lmax <- max(lvec)
    cubes <- unique(round(Lmax/lvec))
    lvec <- unique(Lmax/cubes)
  }

  # some checks
  if (min(lvec) < res){
    message("The smallest scale included in lvec is smaller than the resolution. Consider adjusting lvec.")
  }
  if (max(lvec) < Lmax){
    message("The largest scale included in lvec is smaller than the size of the object.")
  }

  x0 <- min(pts[,1]) - res/2
  y0 <- min(pts[,2]) - res/2
  z0 <- min(pts[,3]) - res/2


  n <- sapply(cubes, function(n) cell_count_3d(pts, x0, x0 + Lmax, y0, y0 + Lmax, z0, z0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(lvec))
  fd <- -as.numeric(coef(mod)[2])

  # plot
  if (plot) {
    if (is(data, "RasterLayer")) {
      plot(data, axes=FALSE)
    } else {
      plot(mesh_to_2d(data), asp=1, type="l", axes=FALSE, xlim=c(x0, x0 + Lmax), ylim=c(y0, y0 + Lmax))
    }
    rect(x0, y0, x0 + lvec, y0 + lvec, border="red")
    axis(1)
    axis(2, las=2)
  }
  # output
  if (keep_data) {
    return(list(D = fd, lvec = lvec, data = data.frame(l = lvec, n = n), method = "cubes"))
  } else {
    return(fd)
  }
}
