#' Calculate fractal Dimension using the height variation method
#'
#' @param data Digital elevation model of class RasterLayer or dataframe (output of hvar function)
#' @param lvec Vector of scales to use for calculation.
#' @param regmethod Method to use for linear regression between scale (lvec) and height range. One of `raw` (all data), `mean` (default) `median` or `ends` (minimum and maximum scale only)
#' @param plot Logical. Show plot of scales relative to data?
#' @param keep_data Keep the data used for fd calculation? defaults to FALSE
#' @param parallel Logical. Use parallel processing? Note: parallel must be installed.
#' @param ncores Number of cores to use when parallel = TRUE.
#'
#' @return A value for fractal dimension, typically between 2 and 3 or a list if keep_data = TRUE.
#' @export
#'
#' @details Calculates fractal dimension using the height variation regression.
#' If `lvec` is not specified, a default based on resolution and extent will be used.
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
#' dem <- habtools::dem_crop(horseshoe, x0 = -469, y0 = 1267, L = 2, plot = TRUE)
#' fd_hvar(dem, lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd_hvar(dem, regmethod = "mean", plot = TRUE, keep_data = TRUE)
#' fd_hvar(dem, regmethod = "median", plot = TRUE, keep_data = TRUE)
#' fd_hvar(dem)
#'

fd_hvar <- function(data, lvec, regmethod = "mean", keep_data = FALSE, plot = FALSE,
                    parallel = FALSE,
                    ncores = (parallel::detectCores()-1)) {

  if (!is.data.frame(data)) {
    L0 <- min(raster::res(data))
    L <- min(dim(data)[1:2] * L0)

    if (missing(lvec)) {
      lvec <- L / 2^(0:20)
      lvec <- sort(lvec[lvec > L0*5])
      message(paste0("lvec is set to c(", toString(round(lvec, 3)), ")."))
    } else {
      lvec <- sort(lvec)
    }
  }

  out <-
    lapply(lvec, function(l){
      list <- dem_split(data, l, parallel = parallel, ncores = ncores)
      h <- sapply(list, function(x){ diff(range(x[], na.rm = T)) })
      data.frame(l = l, h = h)
    }) %>% dplyr::bind_rows()

  out <- log10(out)
  out <- out[is.finite(rowSums(out)),]
  data_mean <- aggregate(h ~ l, out, mean)
  data_median <- aggregate(h ~ l, out, median)
  data_ends <- data_mean[c(1, nrow(data_median)),]

  if (regmethod == "raw") {
    mod <- lm(h ~ l, out)
    dt <- out
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
    x0 <- raster::extent(data)[1]
    y0 <- raster::extent(data)[3]
    raster::plot(data, axes=FALSE)
    rect(x0, y0, x0 + lvec, y0 + lvec, border="red")
    axis(1)
    axis(2, las=2)
  }

  # output
  if (keep_data) {
    return(list(D = unname(f), lvec = lvec, data = 10^dt, method = "hvar"))
  } else {
    return(unname(f))
  }
}
