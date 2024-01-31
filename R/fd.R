#' Fractal dimension
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param lvec scales to use for calculation.
#' @param method If data is a RasterLayer, possible methods are:"hvar", "area", "sd", and "cubes" (defaults to "hvar").
#' If data is a mesh3d, possible methods are "cubes" and "area" (defaults to "cubes").
#' @param keep_data Logical. Keep data? Default is FALSE.
#' @param ... Arguments from other fd_ functions.
#' @seealso [fd_hvar()]
#' @seealso [fd_area()]
#' @seealso [fd_sd()]
#' @seealso [fd_cubes()]
#' @return A value for fractal dimension, typically between 2 and 3.
#' @export
#'
#' @details Calculates fractal dimension using the specified method. Note that methods are distinctly different and should not be mixed when comparing values for multiple objects.
#' The `cubes` method is not recommended if the height range is much smaller than the extent of a 3d object or DEM, which is typically the case for DEMs.
#'
#' @importFrom methods as
#'
#' @examples
#' library(habtools)
#' dem <- crop_dem(horseshoe, x0 = -469, y0 = 1267, L = 2, plot = TRUE)
#' fd(dem, method = "hvar", lvec = c(0.125, 0.25, 0.5, 1, 2))
#' fd(dem, method = "area", diagnose = TRUE)
#' fd(dem, method = "sd")
#' fd(mcap, method = "cubes",  plot = TRUE)
#' fd(dem, method = "sd")
#'
fd <- function(data,  method, lvec, keep_data = FALSE, diagnose = FALSE, ...) {

  if (diagnose) {
    keep_data <- TRUE
  }

  if (is(data, "RasterLayer")) {

    if (missing(method)) method <- "hvar"

    # calculate fd
    if (method == "hvar") {
      f <- fd_hvar(data, lvec = lvec, keep_data = keep_data, ...)
    } else if (method == "sd") {
      f <- fd_sd(data, lvec = lvec, keep_data = keep_data, ...)
    } else if (method == "area") {
      f <- fd_area(data, lvec = lvec, keep_data = keep_data, ...)
    } else if (method == "cubes") {
      f <- fd_cubes(data, lvec = lvec, keep_data = keep_data, ...)
    } else {
      stop("Please check appropriate method options.")
    }

  } else if (is(data, "mesh3d")) {
    if (missing(method)) method <- "cubes"
    if (method == "cubes") {
      f <- fd_cubes(data, lvec = lvec, keep_data = keep_data, ...)
    } else if (method == "area") {
      f <- fd_area(data, lvec = lvec, keep_data = keep_data, ...)
    } else {
      stop("Please check appropriate method options.")
    }
  }

  if (diagnose) {
    dta <- f[["data"]]
    dval <- f[["fd"]]

    f <- diff(log10(dta[,2])) / diff(log10(dta[,1]))
    if (method == "area") {
      f <- 2 - f
    } else if (method == "cubes") {
      f <- -f
    } else {
      f <- 3 - f
    }

    plot(dta[,2] ~ dta[,1], xlab = colnames(dta)[1], ylab = colnames(dta)[2], log="xy", type="l",
         lty=2, col="grey", axes = FALSE, main = paste0('Method: "', method, '"'))
    axis(1)
    axis(2, las=2)
    points(dta[,2] ~ dta[,1])
    text(midv(dta[,1]), midv(dta[,2]), round(f, 2), cex=0.8)
    pred <- predict(lm(log10(dta[,2]) ~ log10(dta[,1])))
    lines(dta[,1], 10^pred, lty = 1, col = "red")
    if (method %in% c("hvar", "sd")) {
      legend("bottomright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
    } else {
      legend("topright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
    }
    return(list(D = unname(dval), data = dta, var = sd(f)))
  } else {
    return(f)
  }
}

midv <- function(v) { v[-length(v)] + diff(v)/2 }

