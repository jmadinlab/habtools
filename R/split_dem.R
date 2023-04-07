#' Split RasterLayer into smaller tiles
#'
#' @param data RasterLayer
#' @param size size of tiles, relative to the unit of the RasterLayer
#'
#' @return List of RasterLayers
#' @export
#'
#' @examples
#' L <- res(data)[1] * nrow(data) # size of horseshoe = 8m
#' size <- 2 # size of target tiles
#' (L / size)^2 # number of target tiles = 16
#' dem_list <- split_dem(horseshoe, 2)
#' length(dem_list)
#'
split_dem <- function(data, size) {
  L <- res(data)[1] * nrow(data)
  if(!round(L/size, 5) == round(L/size)){
    stop("Specify size so that the RasterLayer can be divided into equal tiles")
  }
  t <- nrow(data) / (L/size)
  a <- aggregate(data, t)
  p <- as(a, 'SpatialPolygons')
  lapply(seq_along(p), function(i) crop(horseshoe, p[i]))
  }
