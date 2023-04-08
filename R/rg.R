#' Rugosity
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param x Bottom-left of bounding box.
#' @param y Bottom-left of bounding box.
#' @param L Bounding box extent (i.e., side length).
#' @param L0 Grain or resolution of calculation.
#' @param method If data is a RasterLayer methods "hvar" or "area" are allowed. Defaults to "hvar".
#' @param parallel Use parallel processing (Defaults to FALSE)
#' @param ncores number of cores to use if parallel = T.
#' (Defaults to umber of available cores - 1)
#' @details
#' Rugosity is defined as the surface area divided by the planar area. For digital elevation models, we include two methods: "hvar" and "area". \cr
#' The "hvar" method for calculating rugosity is described in Torres-Pulliza et al. (2004) and is based on height variations. \cr
#' The "area" method uses the [sp::surfaceArea()] function and is detailed in Jenness (2004). \cr
#' `method` is ignored if `data` is a mesh3D object.
#' In that case the function uses [Rvcg::vcgArea()] to calculate surface area of a triangular mesh of class mesh3d.
#' @references
#' Calculating Landscape Surface Area from Digital Elevation Models, Jeff S. Jenness Wildlife Society Bulletin, Vol. 32, No. 3 (Autumn, 2004), pp. 829-839n \cr
#' Torres-Pulliza, D., Dornelas, M.A., Pizarro, O. et al. A geometric basis for surface habitat complexity and biodiversity. Nat Ecol Evol 4, 1495–1501 (2020). https://doi.org/10.1038/s41559-020-1281-8
#' @return Rugosity value
#' @import raster
#' @export
#'
#' @examples
#' rg(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.1)
#' rg(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.1, method = "area")
#' rg(mcap, L0 = 0.01)
#'
rg <- function(data, x, y, L, L0, method = "hvar", parallel = FALSE,
               ncores = (parallel::detectCores() - 1)) {
  if (class(data) == "RasterLayer") {
    if (missing(x)) x <- raster::xmin(data)
    if (missing(y)) y <- raster::ymin(data)
    if (missing(L)) L <- min(dim(data)[1:2] * raster::res(data))

    if (L < min(dim(data)[1:2] * raster::res(data))) {
      b <- as(raster::extent(x, x + L, y, y + L), 'SpatialPolygons')
      raster::crs(b) <- raster::crs(data)
      data <- raster::crop(data, b)
    }

    if (method == "hvar") {

      dem_list <- split_dem(data, L0)

      if (parallel){
        hs <- parallel::mclapply(dem_list, hr,
                                 mc.cores = ncores) %>% unlist()
      } else{
        hs <- sapply(dem_list, hr)
      }
      rg <- mean(sqrt((hs^2) / (2 * L0^2) + 1), na.rm = T)

    } else if (method =="area") {
      fac <- round(L0/raster::res(data)[1])
      a <- raster::aggregate(data, fac)
      mat <- as.matrix(a)/L0
      rg <- sp::surfaceArea(mat)/(dim(mat)[1] * dim(mat)[2])
    } else {
      stop("method can only be 'hvar' or 'area'")
    }
  } else if (class(data) == "mesh3d") {
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
  } else {
    stop("data must be of class RasterLayer or mesh3d with triangular mesh")
  }
  return(rg)
}

