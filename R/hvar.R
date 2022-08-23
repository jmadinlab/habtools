#' Height Variation in cells at different scales
#'
#' @param dem Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param L0 Resolution
#' @param parallel TRUE or FALSE. Use parallel computation?
#' @param ncores number of cores to use when parallel = TRUE.
#' @param n number of scales to use
#'
#' @return A `data.frame` containing height ranges of cells at different scales.
#' @export
#'
#' @details
#' This function is used for calculating fractal dimension using the
#' height variation method.
#' It is separated from the `fd` function because it takes a while to run, and so it is useful
#' to save the output before using the `fd` function.
#'
#' @examples
#'
#' hvar(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.5)

hvar <- function(dem, x, y, L0, L, n = 5,
                 parallel = FALSE,
                 ncores = (parallel::detectCores()-1)) {

  Lvec <- 10^seq(log10(L0), log10(L), (log10(L)-log10(L0))/n)
  hvar <-
    lapply(Lvec, function(L0){
      cells <- expand.grid(x = seq(x, x + L - L0, L0),
                           y = seq(y, y + L - L0, L0))
      df  <- terra::as.data.frame(dem, xy = TRUE)
      if (parallel){
        h0 <- parallel::mclapply(1:nrow(cells), function(i){
          hr(df, x = cells$x[i],  y = cells$y[i], L = L0, plot = FALSE)
        }, mc.cores = ncores) %>% unlist()
      } else{
        h0 <- lapply(1:nrow(cells), function(i){
          hr(df, x = cells$x[i],  y = cells$y[i], L = L0, plot = FALSE)
        }) %>% unlist()
      }
      data.frame(L0 = L0, H0 = h0)
    }) %>% dplyr::bind_rows()
  return(hvar)
}
