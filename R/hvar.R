#' Height Variation in cells at different scales
#'
#' @param dem Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param Lvec A vector of scales to collate.
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
#' hvar(horseshoe, x=-470, y=1266, L=2, Lvec=c(2, 1, 0.5))

hvar <- function(dem, x, y, L, Lvec,
                 parallel = FALSE,
                 ncores = (parallel::detectCores()-1)) {
  if (parallel == FALSE){
    hvar <-
      lapply(Lvec, function(L0){
        cells <- expand.grid(x=seq(x, x+L-L0, L0), y=seq(y, y+L-L0, L0))
        H0 <- mapply(hr, x=cells$x, y=cells$y, MoreArgs=list(L=L0, data=dem))
        data.frame(L0=L0, H0=H0)
      }) %>% dplyr::bind_rows()
  } else if (parallel == T) {
    hvar <-
      lapply(Lvec, function(L0){
        cells <- expand.grid(x = seq(x, x + L - L0, L0),
                             y = seq(y, y + L - L0, L0))
        H0 <- parallel::mclapply(1:nrow(cells), function(i){
          hr(x = cells[i, "x"], y = cells[i, "y"], L = L0, data = dem)
        }, mc.cores = ncores) %>% simplify2array()
        data.frame(L0=L0, H0=H0)
      }) %>% dplyr::bind_rows()
  }
  return(hvar)
}
