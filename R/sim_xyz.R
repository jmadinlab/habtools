#' Simulate z values
#'
#' Simulates z-values based on a spatial covariance distribution.
#' Warning: this function can currently only handle a small number for n.
#'
#' @param n Number of rows and cols.
#' @param mu Average z.
#' @param phisq Parameter.
#' @param sigmasq parameter: maximum covariance
#' @param dem T or F
#'
#' @return a xyz dataframe or raster (if dem = T)
#' @export
#'
#' @examples
#' library(raster)
#' d <- sim_xyz(10, 6, 0.1, 2, dem = TRUE)
#' plot(d)

sim_xyz <- function(n, mu, phisq, sigmasq, dem = FALSE){
  gr <- expand.grid(x = 1:n, y = 1:n)
  dist <- as.matrix(dist(gr))
  Sigma <- sigmasq*(exp(-phisq * dist^2))
  gr$z <- MASS::mvrnorm(n = 1, rep(mu, n*n), Sigma)
  gr <- as.data.frame(gr)
  if (dem == TRUE) {
    raster::rasterFromXYZ(gr)
  } else {
    as.data.frame(gr)
  }
}
