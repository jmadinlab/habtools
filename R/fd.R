#' Fractal Dimension
#'
#' @param data A `hvar` `data.frame`.
#' @param method One of `raw`, `mean` (default), `median` or `ends`
#' @param plot Diagnostic plot
#'
#' @return A value for fractal dimension, hopefully between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the height variation method.
#' `data` can be any `data.frame` with columns labeled `L0` and `H0` for
#' grid cell length and height range of that cell, respectively.
#' A rule of thumb is that `L0` should range two orders of magnitude.
#' However, large ranges also
#' average-out fractal dimension of a surface that might have
#' phase transitions, and therefore a thorough exploration of height ranges is suggested using the `diagnostic` plot, which plots all the methods.
#' These methods will converge on purely fractal surfaces.
#'
#' @examples
#' data <- hvar(horseshoe, x=-470, y=1266, L=2, Lvec=c(2, 1, 0.5, 0.25))
#' fd(data)
#' fd(data, method="raw")
#' fd(data, method="raw", plot=TRUE)
#'
fd <- function(data, method="mean", plot=FALSE) {
  data <- log10(data)
  data <- data[is.finite(rowSums(data)),]
  data_mean <- aggregate(H0 ~ L0, data, mean)
  data_median <- aggregate(H0 ~ L0, data, median)
  data_ends <- data_mean[c(1, nrow(data_mean)),]

  if (method == "raw") fd <- 3 - coef(lm(H0 ~ L0, data))[2]
  if (method == "mean") fd <- 3 - coef(lm(H0 ~ L0, data_mean))[2]
  if (method == "median") fd <- 3 - coef(lm(H0 ~ L0, data_median))[2]
  if (method == "ends") fd <- 3 - coef(lm(H0 ~ L0, data_ends))[2]

  if (plot) {
    plot(data, xlab="log10(L0)", ylab="log10(H0)")
    lines(data_mean, lty=1)
    lines(data_median, lty=2)
    lines(data_ends, lty=3)
    legend("bottomright", legend=c("raw", "mean", "median", "ends"), pch=c(1, NA, NA, NA), lty=c(NA, 1, 2, 3), bty="n")
  }

  return(as.vector(fd))
}
