#' Simulate z values
#'
#' @param n number of rows or cols
#' @param mu average z
#' @param phisq parameter
#' @param sigmasq parameter: maximum covariance
#' @param dem T or F
#'
#' @return a xyz dataframe or raster (if dem = T)
#' @export
#'
#' @examples
#' d <- sim_xyz(10, 6, 0.1, 2, T)
#' plot(d)

sim_xyz <- function(n, mu, phisq, sigmasq, dem = FALSE){
  gr <- expand.grid(x = 1:n, y = 1:n)
  dist <- as.matrix(dist(gr))
  Sigma <- sigmasq*(exp(-phisq * dist^2))
  gr$z <- MASS::mvrnorm(n = 1, rep(mu, n*n), Sigma)
  gr <- as.data.frame(gr)
  if (dem == TRUE) {
    rasterFromXYZ(gr)
  } else {
    as.data.frame(gr)
  }
}
