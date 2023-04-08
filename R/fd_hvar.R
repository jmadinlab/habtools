#' Fractal Dimension using the height variation method
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param regmethod One of `raw`, `mean` (default), `median` or `ends`
#' @param plot Diagnostic plot
#'
#' @return A value for fractal dimension, normally between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the height variation regression.
#' `data` can be any `data.frame` with columns labeled `l` and `h` for
#' grid cell length and height range of that cell, respectively.
#' A rule of thumb is that `l` should range an order of magnitude.
#' However, large ranges also
#' average-out fractal dimension of a surface that might have
#' phase transitions, and therefore a thorough exploration of height ranges is suggested using the `diagnostic` plot, which plots all the regressions.
#' These regressions will converge on purely fractal surfaces.
#'
#' @examples
#' data <- hvar(horseshoe, x=-470, y=1266, L=2, Lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd_hvar(data)
#' fd_hvar(data, regmethod = "ends")
#' fd_hvar(data, regmethod = "median", plot=TRUE)
#'
fd_hvar <- function(data, regmethod = "mean", plot = FALSE) {
  data <- log10(data)
  data <- data[is.finite(rowSums(data)),]
  data_mean <- aggregate(h ~ l, data, mean)
  data_median <- aggregate(h ~ l, data, median)
  data_ends <- data_mean[c(1, nrow(data_mean)),]

  if (regmethod == "raw") fd <- 3 - coef(lm(h ~ l, data))[2]
  if (regmethod == "mean") fd <- 3 - coef(lm(h ~ l, data_mean))[2]
  if (regmethod == "median") fd <- 3 - coef(lm(h ~ l, data_median))[2]
  if (regmethod == "ends") fd <- 3 - coef(lm(h ~ l, data_ends))[2]


  if (plot) {
    plot(data, xlab="log10(l)", ylab="log10(h)")
    lines(data_mean, lty=1)
    lines(data_median, lty=2)
    lines(data_ends, lty=3)
    legend("bottomright", legend=c("raw", "mean", "median", "ends"), pch=c(1, NA, NA, NA), lty=c(NA, 1, 2, 3), bty="n")
  }

  return(as.vector(fd))
}
