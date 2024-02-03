#' Fractal dimension
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param lvec scales to use for calculation.
#' @param method If data is a RasterLayer, possible methods are:"hvar", "area", "sd", and "cubes" (defaults to "hvar").
#' If data is a mesh3d, possible methods are "cubes" and "area" (defaults to "cubes").
#' @param keep_data Logical. Keep data? Default is FALSE.
#' @param diagnose Logical. Show diagnostic plot and metrics?
#' @param ... Arguments from other fd_ functions.
#' @seealso [fd_hvar()]
#' @seealso [fd_area()]
#' @seealso [fd_sd()]
#' @seealso [fd_cubes()]
#' @seealso [fd_diagnose()]
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
#' fd(mcap2, method = "cubes",  plot = TRUE)
#' fd(mcap2, method = "area",  diagnose = TRUE)
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
    return(fd_diagnose(f))
  } else {
    return(f)
  }
}

midv <- function(v) { v[-length(v)] + diff(v)/2 }

