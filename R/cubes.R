#' Cube counting for 3D mesh
#'
#' @param mesh 3D mesh with vertices
#' @param L0 The grain or resolution (i.e., smallest cube size)
#' @param plot Plot number of filled cubes at different scales
#'
#' @return The fractal dimension value
#' @export
#'
#' @examples
#' cubes(mcap)
#'
cubes <- function(mesh, L0, plot=FALSE) {

  pts <- data.frame(t(mesh$vb)[,1:3])
  names(pts) <- c("x", "y", "z")

  if (missing(L0)) {
    dts <- as.matrix(dist(pts, diag=FALSE, upper=TRUE))
    dts[dts==0] <- NA
    L0 <- apply(dts, 1, min, na.rm=TRUE)
    L0 <- max(L0)*sqrt(2)
  }

  max_size <- max(diff(apply(pts, 2, range))) + 2 * L0
  cubes <- c(1, 2, 4, 8, 16, 32, 64, 128)
  sizes <- max_size/cubes
  cubes <- cubes[sizes > L0]
  sizes <- sizes[sizes > L0]

  x0 <- min(pts[,1]) - L0
  y0 <- min(pts[,2]) - L0
  z0 <- min(pts[,3]) - L0

  n <- lapply(cubes, function(n) cell_count(pts, x0, x0 + max_size, y0, y0 + max_size, z0, z0 + max_size, n))
  n <- unlist(n)

  mod <- lm(log10(n) ~ log10(sizes))
  fd <- -as.numeric(coef(mod)[2])

  if (plot) {
    plot(log10(n) ~ log10(sizes))
    abline(mod)
  }

  return(list(cubes=data.frame(L0=sizes, n=n), fd=fd))
}
