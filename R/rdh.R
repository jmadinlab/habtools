#' Calculate Rugosity, fractal dimension and height for a DEM
#'
#' @param data A dem
#' @param x minimum x-value
#' @param y minimum y-value
#' @param L Extent
#' @param L0 Minimum resolution
#' @param parallel use parallel processing
#' @param ncores number of cores to use if parallel = TRUE
#'
#' @return a dataframe with the three complexity measures
#' @export
#'
#' @examples
#' rdh(horseshoe, x=-470, y=1266, L=2, L0=0.5)
rdh <- function(data, x, y, L, L0, parallel = FALSE, ncores = (parallel::detectCores()-1)){

  hv <- hvar(data, x = x, y = y, L = L, L0 = L0,
             parallel = parallel, ncores = ncores)
  hs <- hv[hv$L0 == min(hv$L0), "H0"]
  rg <- mean(sqrt((hs^2) / (2 * L0^2) + 1), na.rm = T)
  d <- fd(hv)
  h <- hv[hv$L0 == L, "H0"]

  data.frame(R = rg, D = d, H = h)

}
