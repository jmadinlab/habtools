#' Height Variation in cells at different scales
#'
#' @param data Digital elevation model of class RasterLayer.
#' @param lvec Scales to use for calculation.
#' @param parallel Logical. Use parallel processing? Note: parallel must be installed.
#' @param ncores Number of cores to use when parallel = TRUE.
#'
#' @return A `data.frame` containing height ranges of cells at different scales.
#' @export
#'
#' @details
#' This function is used for calculating fractal dimension using the
#' height variation method.
#'
#' @examples
#'
#' hvar(horseshoe, lvec = c(0.125, 0.25, 0.5, 1))

hvar <- function(data, lvec,
                 parallel = FALSE,
                 ncores = (parallel::detectCores()-1)) {


  if (sum(is.na(values(data))) > 0) {
    message(paste0("data contains ", sum(is.na(values(data))), " NA values. Results may be biased."))
  }


  L0 <- min(raster::res(data))
  L <- min(dim(data)[1:2] * L0)

  if (missing(lvec)) {
    lvec <- 2^(seq(log2(L), log2(L0*10)))
    lvec <- sort(lvec)
    print(paste0("lvec is set to c(", toString(lvec), ")."))
  } else {
    lvec <- sort(lvec)
  }

  hvar <-
    lapply(lvec, function(l){
      list <- split_dem(data, l, parallel = parallel, ncores = ncores)
      h <- sapply(list, hr)
      data.frame(l = l, h = h)
    }) %>% dplyr::bind_rows()

  return(hvar)
}
