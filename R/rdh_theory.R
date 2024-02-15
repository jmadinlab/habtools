#' Calculate metric based on geometric plane equation
#'
#' @description Calculates either rugosity, fractal dimension or height range based on the
#' other two variables.
#'
#' @param r,d,h two of the three variables to calculate the third.
#' @param L Extent.
#' @param L0 Resolution.
#'
#' @details This function uses the geometric plane equation from Torres-Pulliza et al. (2020) to calculate one of rugosity,
#' fractal dimension or height range based on the other two variables.
#'
#' @return A value corresponding one of the three variables not given to the function.
#' @export
#'
#' @references Torres-Pulliza D, Dornelas M, Pizarro O, Bewley M, Blowes SA, Boutros N, Brambilla V, Chase TJ, Frank G, Friedman A, Hoogenboom MO, Williams S, Zawada KJA, Madin JS (2020) A geometric basis for surface habitat complexity and biodiversity. *Nature Ecology & Evolution* 4:1495-1501. [https://doi.org/10.1038/s41559-020-1281-8](https://doi.org/10.1038/s41559-020-1281-8)
#'
#' @examples
#' rdh_theory(r=4, h=1, L=1, L0=0.01)
#' rdh_theory(d=2.36928, h=1, L=1, L0=0.01)
#' rdh_theory(d=2.36928, r=4, L=1, L0=0.01)

rdh_theory <- function(r, d, h, L, L0) {
  v <- NA
  if (missing(r)) { # R
    if (missing(d) | missing(h)) {
      stop("you need to provide two variables")
    }
    v <- sqrt((exp(log(h) - log(L / L0) * (3 - d)) / (L0 * sqrt(2)))^2 + 1)
    return(list(r=v))
  }
  if (missing(d)) { # D
    if (missing(r) | missing(h)) {
      stop("you need to provide two variables")
    }
    v <- 3 - (log(h) - log(sqrt(2) * L0 * sqrt(r^2 - 1))) / log(L / L0)
    return(list(d=v))
  }
  if (missing(h)) { # H
    if (missing(r) | missing(d)) {
      stop("you need to provide two variables")
    }
    v <- exp(log(L / L0) * (3 - d) + log(sqrt(2) * L0 * sqrt(r^2 - 1)))
    return(list(h=v))
  }
}

