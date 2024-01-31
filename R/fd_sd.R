#' Calculate fractal Dimension using the standard deviation method
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param lvec Scales to use for calculation.
#' @param regmethod Method to use for linear regression between scale (lvec) and height range. One of `raw` (all data), `mean` (default) `median` or `ends` (minimum and maximum scale only)
#' @param plot Logical. Show plot?
#' @param keep_data Logical. Keep the data used for fd calculation? Defaults to FALSE.
#' @param parallel Logical. Use parallel processing? Note: parallel must be installed.
#' @param ncores Number of cores to use when parallel = TRUE.
#'
#' @return A value for fractal dimension, normally between 2 and 3.
#' @export
#'
#' @importFrom stats sd

#'
#' @details Calculates fractal dimension using the standard deviation method,
#' an analogue of the variation method, but using the standard deviation in height per grid cell instead of the full height range.
#' A rule of thumb is that `l` should range at least an order of magnitude.
#' However, large ranges also average-out fractal dimension of a surface that might have
#' phase transitions, and therefore a thorough exploration of height ranges is suggested using the `plot`.
#' `regmethod` specifies whether data is summarized by taking the mean or median of height ranges across scales or all data is used.
#' `regmethod` "raw" is not recommended because the regression will give much more weight to the lower scales that include more points and likely underestimate D.
#'
#' @examples
#' fd_sd(horseshoe, x=-470, y=1266, L=2, lvec = c(0.05, 0.1, 0.25, 0.5, 1, 2))
#'

fd_sd <- function(data, x, y, lvec, L,
                  regmethod = "mean", keep_data = FALSE, plot = FALSE,
                  parallel = FALSE,
                  ncores = (parallel::detectCores()-1)) {

  L0 <- min(raster::res(data))
  L <- min(dim(data)[1:2] * L0)

  if (missing(lvec)) {
    lvec <- 2/unique(round(2^seq(log2(L/L),log2(L/(L0*10)), length.out = 10)))
    lvec <- sort(lvec)
    print(paste0("lvec is set to c(", toString(round(lvec, 3)), ")."))
  } else {
    lvec <- sort(lvec)
  }

  out <-
    lapply(lvec, function(l){
      list <- split_dem(data, l, parallel = parallel, ncores = ncores)
      sd <- sapply(list, function(x){sd(x[], na.rm = T)})
      data.frame(l = l, sd = sd)
    }) %>% dplyr::bind_rows()

  out <- log10(out)
  out <- out[is.finite(rowSums(out)),]
  out_mean <- aggregate(sd ~ l, out, mean)
  out_median <- aggregate(sd ~ l, out, median)
  out_ends <- out_mean[c(1, nrow(out_median)),]

  if (regmethod == "raw") {
    mod <- lm(sd ~ l, out)
    dt <- out
  } else if (regmethod == "mean") {
    mod <- lm(sd ~ l, out_mean)
    dt <- out_mean
  } else if (regmethod == "median") {
    mod <- lm(sd ~ l, out_median)
    dt <- out_median
  } else if (regmethod == "ends") {
    mod <- lm(sd ~ l, out_ends)
    dt <- out_ends
  } else {
    stop("Specified regmethod is incorrect.")
  }

  f <- 3 - coef(mod)[2]

  # plot
  if (plot) {
    plot(dt, xlab = "log10(l)", ylab = "log10(sd)")
    abline(mod, lty = 1)
  }

  # output
  if (keep_data) {
    return(list(fd = unname(f), data = 10^dt))
  } else {
    return(unname(f))
  }
}


