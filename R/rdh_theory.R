#' Calculate metric based on geometric plane equation
#'
#' @description Calculates either rugosity, fractal dimension or height
#' range based on the other two variables.
#'
#' @param R,D,H Two of the three variables to calculate the third.
#' @param L Extent.
#' @param L0 Resolution.
#'
#' @details This function uses the geometric plane equation from Torres-Pulliza
#' et al. (2020) to calculate one of rugosity, fractal dimension or height
#' range based on the other two variables.
#'
#' @return A value corresponding one of the three variables not given
#' to the function.
#'
#' @export
#'
#' @references Torres-Pulliza D, Dornelas M, Pizarro O, Bewley M, Blowes SA, Boutros N, Brambilla V, Chase TJ, Frank G, Friedman A, Hoogenboom MO, Williams S, Zawada KJA, Madin JS (2020) A geometric basis for surface habitat complexity and biodiversity. *Nature Ecology & Evolution* 4:1495-1501. \https://doi.org/10.1038/s41559-020-1281-8
#'
#' @examples
#' rdh_theory(R=4, H=1, L=1, L0=0.01)
#' rdh_theory(D=2.36928, H=1, L=1, L0=0.01)
#' rdh_theory(D=2.36928, R=4, L=1, L0=0.01)

rdh_theory <- function(R, D, H, L, L0) {
  v <- NA
  if (missing(R)) { # R
    if (missing(D) | missing(H)) {
      stop("you need to provide two variables")
    }
    v <- sqrt((exp(log(H) - log(L / L0) * (3 - D)) / (L0 * sqrt(2)))^2 + 1)
    return(list(R=v))
  }
  if (missing(D)) { # D
    if (missing(R) | missing(H)) {
      stop("you need to provide two variables")
    }
    v <- 3 - (log(H) - log(sqrt(2) * L0 * sqrt(R^2 - 1))) / log(L / L0)
    return(list(D=v))
  }
  if (missing(H)) { # H
    if (missing(R) | missing(D)) {
      stop("You need to provide two variables")
    }
    v <- exp(log(L / L0) * (3 - D) + log(sqrt(2) * L0 * sqrt(R^2 - 1)))
    return(list(H=v))
  }
}

