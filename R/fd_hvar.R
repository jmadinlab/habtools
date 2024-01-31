#' Calculate fractal Dimension using the height variation method
#'
#' @param data Digital elevation model of class RasterLayer or dataframe (output of hvar function)
#' @param lvec Scales to use for calculation.
#' @param regmethod Method to use for linear regression between scale (lvec) and height range. One of `raw` (all data), `mean` (default) `median` or `ends` (minimum and maximum scale only)
#' @param plot Diagnostic plot
#' @param keep_data Keep the data used for fd calculation? defaults to FALSE
#' @param ... Additional arguments of see [hvar()]
#'
#' @return A value for fractal dimension, normally between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the height variation regression.
#' `data` can be a DEM or a `data.frame` with columns labeled `l` and `h` for
#' grid cell length and height range of that cell, respectively (output of [hvar()]).
#' A rule of thumb is that `l` should range an order of magnitude.
#' However, large ranges also
#' average-out fractal dimension of a surface that might have
#' phase transitions, and therefore a thorough exploration of height ranges is suggested using the `plot`.
#' `regmethod` specifies whether data is summarized by taking the mean or median of height ranges across scales or all data is used.
#' `regmethod` "raw" is not recommended because the regression will give much more weight to the lower scales that include more points and likely underestimate D.
#'
#' @examples
#' dem <- habtools::crop_dem(horseshoe, x0 = -469, y0 = 1267, L = 2, plot = TRUE)
#' data <- hvar(dem, L=2, lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd_hvar(data)
#' fd_hvar(dem, lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd_hvar(data, regmethod = "mean", plot = TRUE, keep_data = TRUE)
#' fd_hvar(data, regmethod = "ends")
#' fd_hvar(data, regmethod = "median", plot = TRUE, keep_data = TRUE)
#' fd_hvar(dem)
#'
fd_hvar <- function(data, lvec, regmethod = "mean", keep_data = FALSE, plot = FALSE, ...) {

  if (!is.data.frame(data)) {
    L0 <- min(raster::res(data))
    L <- min(dim(data)[1:2] * L0)

    if (missing(lvec)) {
      lvec <- 2/unique(round(2^seq(log2(L/L),log2(L/(L0*10)), length.out = 10)))
      lvec <- sort(lvec)
      print(paste0("lvec is set to c(", toString(round(lvec, 3)), ")."))
    } else {
      lvec <- sort(lvec)
    }
    data <- hvar(data, lvec = lvec, ...)
  }

  data <- log10(data)
  data <- data[is.finite(rowSums(data)),]
  data_mean <- aggregate(h ~ l, data, mean)
  data_median <- aggregate(h ~ l, data, median)
  data_ends <- data_mean[c(1, nrow(data_median)),]

  if (regmethod == "raw") {
    mod <- lm(h ~ l, data)
    dt <- data
    } else if (regmethod == "mean") {
      mod <- lm(h ~ l, data_mean)
      dt <- data_mean
    } else if (regmethod == "median") {
      mod <- lm(h ~ l, data_median)
      dt <- data_median
    } else if (regmethod == "ends") {
      mod <- lm(h ~ l, data_ends)
      dt <- data_ends
    } else {
      stop("Specified regmethod is incorrect.")
    }

  f <- 3 - coef(mod)[2]

  # plot
  if (plot) {
    plot(dt, xlab = "log10(l)", ylab = "log10(h)")
    abline(mod, lty = 1)
  }

  # output
  if (keep_data) {
    return(list(fd = unname(f), data = 10^dt))
  } else {
    return(unname(f))
  }
}
