#' Calculates rugosity, fractal dimension or height range based the other two variables
#'
#' @param r,d,h two of the three variables to calculate the third.
#' @param L extent.
#' @param L0 resolution.
#'
#' @details This function uses a plane equation to calculate either rugosity,
#' fractal dimension or height range based on two of these variables.
#'
#' @return A value corresponding one of the three variables not given to the function.
#' @export
#'
#' @references Torres-Pulliza D, Dornelas M, Pizarro O, Bewley M, Blowes SA, Boutros N, Brambilla V, Chase TJ, Frank G, Friedman A, Hoogenboom MO, Williams S, Zawada KJA, Madin JS (2020) A geometric basis for surface habitat complexity and biodiversity. *Nature Ecology & Evolution* 4:1495-1501. https://doi.org/10.1038/s41559-020-1281-8
#'
#' @examples
#' rdh_theory(r=3, h=1, L=2, L0=0.01)
#' rdh_theory(d=2.392472, h=1, L=2, L0=0.01)
#' rdh_theory(d=2.392472, r=3, L=2, L0=0.01)

rdh_theory <- function(r, d, h, L, L0) {
  v <- NA
  if (missing(r)) { # R
    if (missing(d) | missing(h)) {
      stop("you need to provide two variables")
    }
    v <- sqrt((exp(log(h) - log(L / L0) * (3 - d)) / (L0 * sqrt(2)))^2 + 1)
  }
  if (missing(d)) { # D
    if (missing(r) | missing(h)) {
      stop("you need to provide two variables")
    }
    v <- 3 - (log(h) - log(sqrt(2) * L0 * sqrt(r^2 - 1))) / log(L / L0)
  }
  if (missing(h)) { # H
    if (missing(r) | missing(d)) {
      stop("you need to provide two variables")
    }
    v <- exp(log(L / L0) * (3 - d) + log(sqrt(2) * L0 * sqrt(r^2 - 1)))
  }

  return(v)
}

