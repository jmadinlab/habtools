#' Cube counting for 3D mesh
#'
#' @param mesh 3D mesh with vertices
#' @param L0 The grain or resolution (i.e., smallest cube size)
#' @param Lmax The grain of the biggest cube (default: cube encapsulating mesh)
#' @param n The number of cube sizes to include.
#' @param plot Plot number of filled cubes at different scales
#'
#' @details This tool creates arrays of cubes of different sizes and counts mesh points that fall within cubes.
#' The number of cubes for each scale can be selected (optionally), but ensure you start with 1 (i.e., a cube that encapsulates the whole mesh) and
#' keep in mind fractals are calculated over log scales. For instance, the default `cubes` is a vector of doubling numbers `c(1, 2, 4, 8, 16, 32, ...)`.
#' The actual array of cubes will be these numbers to the third power.
#' The tool will not allow the number of cubes to cause the cube size to fall below the resolution `L0` of the mesh, because
#' this will increase the chance that smaller cubes fall between mesh points and so underestimate cube counts.
#' If `L0` is not given, it will be calculated as `sqrt(2)` multiplied by the largest nearest neighbor distance of points.
#' Most meshes are not perfectly fractal, and so use the `plot` parameter to look for scale transitions.
#'
#' @return A `data.frame` of number of cubes (`n`) intersecting mesh points at different cube sizes (`L`) and a fractal dimension value. Note that cube size is the length of a side.
#' @export
#' @seealso [fd()] for fractal dimension from variation method
#' @examples
#' cubes(mcap)
#' cubes(mcap, plot=TRUE)
#' cubes(mcap, L0 =  0.05, plot=TRUE)
#'
cubes <- function(mesh, L0, Lmax, plot = FALSE, n = 5) {

  pts <- data.frame(t(mesh$vb)[,1:3])
  names(pts) <- c("x", "y", "z")

  maxres <- max(Rvcg::vcgMeshres(mcap)[[2]])

  if (missing(L0)) {
    L0 <- maxres
  }

  if (L0 < maxres){
    warning("L0 is smaller than recommended.")
  }

  if (missing(Lmax)) {
    Lmax <- max(diff(apply(pts, 2, range))) + 2 * L0
  }


  sizes <- 10^(seq(log10(L0), log10(Lmax), (log10(Lmax) - log10(L0))/n))

  cubes <- unique(round(Lmax/sizes))
  sizes <- Lmax/cubes

  x0 <- min(pts[,1]) - L0
  y0 <- min(pts[,2]) - L0
  z0 <- min(pts[,3]) - L0

  n <- sapply(cubes, function(n) cell_count(pts, x0, x0 + Lmax, y0, y0 + Lmax, z0, z0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(sizes))
  fd <- -as.numeric(coef(mod)[2])

  if (plot) {
    plot(log10(n) ~ log10(sizes))
    abline(mod)
  }

  return(list(cubes = data.frame(L0 = sizes, n = n), fd = fd))
}

