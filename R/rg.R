#' Rugosity
#'
#' @param data Digital elevation model in raster format.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param L0 Grain or resolution of calculation.
#' @param plot Add bounding box to plot.
#' @param method "mesh" or "dem"
#' @param parallel parallel
#' @param ncores number of cores to use if parallel = T
#'
#' @return Rugosity value
#' @export
#'
#' @examples
#' rg(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.5)
#'
#' plot(horseshoe, asp = 1)
#' rg(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.5, plot = TRUE)
#'
#' rg(mcap, L0 = 0.01, method = "mesh")
#'
rg <- function(data, x, y, L, L0, method = "dem", plot = FALSE, parallel = FALSE,
               ncores = (parallel::detectCores() - 1)) {
  if (method == "dem") {
    if (missing(x)) x <- raster::xmin(data)
    if (missing(y)) y <- raster::ymin(data)
    if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))

    cells <- expand.grid(x = seq(x, x + L - L0, L0),
                         y = seq(y, y + L - L0, L0))

    df  <- terra::as.data.frame(data, xy = TRUE)

    if (parallel){
      hs <- parallel::mclapply(1:nrow(cells), function(i){
        hr(df, x = cells$x[i],  y = cells$y[i], L = L0, plot = FALSE)
      }, mc.cores = ncores) %>% unlist()
    } else{
      hs <- lapply(1:nrow(cells), function(i){
        hr(df, x = cells$x[i],  y = cells$y[i], L = L0, plot = FALSE)
      }) %>% unlist()
    }

    #H0 <- mean(hs)
    rg <- mean(sqrt((hs^2) / (2 * L0^2) + 1), na.rm = T)
  }
  if (method == "mesh") {
    res <- Rvcg::vcgMeshres(data)$res[[1]]
    if (missing(L0)){
      L0 <- res
      message(paste("L0 is set to mesh resolution (", L0, ")", sep = ""))
    }
    if (L0 < res) {
      warning("L0 is smaller than mesh resolution")
    }
    if (!L0 == res){
      mesh <- Rvcg::vcgQEdecim(data, edgeLength = L0, silent = T)
    } else {
      mesh <- data
    }
    rg <- Rvcg::vcgArea(mesh)/planar(mesh, silent = T)
  }
  return(rg)
}

