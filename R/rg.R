#' Calculate rugosity
#'
#' @description
#' Rugosity is defined as the surface area divided by the planar area. For digital elevation models, there are two methods: "hvar" and "area".
#' The "hvar" method for calculating rugosity is described in Torres-Pulliza et al. (2004) and is based on height variations.
#' The "area" method uses the [sp::surfaceArea()] function and is detailed in Jenness (2004).
#' `method` is ignored if `data` is a mesh3D object.
#' In that case the function uses [Rvcg::vcgArea()] to calculate surface area of a triangular mesh of class mesh3d.
#'
#' @param data Digital elevation model of class RasterLayer or a triangular mesh of class mesh3d.
#' @param L0 Grain or resolution of calculation.
#' @param method If data is a RasterLayer methods "hvar" or "area" are allowed. Defaults to "hvar".
#' @param parallel Logical. Use parallel processing? Defaults to FALSE.
#' @param ncores Number of cores to use if parallel = TRUE.
#' (Defaults to umber of available cores - 1)
#'
#' @references
#' Calculating Landscape Surface Area from Digital Elevation Models, Jeff S. Jenness Wildlife Society Bulletin, Vol. 32, No. 3 (Autumn, 2004), pp. 829-839n
#' \cr\cr
#' Torres-Pulliza, D., Dornelas, M.A., Pizarro, O. et al. A geometric basis for surface habitat complexity and biodiversity. Nat Ecol Evol 4, 1495–1501 (2020). https://doi.org/10.1038/s41559-020-1281-8
#' @return Rugosity value
#' @import raster
#' @export
#'
#' @examples
#' rg(horseshoe, L0 = 0.1)
#' rg(mcap, L0 = 0.01)
#'
rg <- function(data, L0, method = "area", parallel = FALSE,
               ncores = (parallel::detectCores() - 1)) {
  if (is(data, "RasterLayer")) {

    if (sum(is.na(values(data))) > 0) {
      message(paste0("data contains ", sum(is.na(values(data))), " NA values. Results may be biased."))
    }

    if (method == "hvar") {

      if (missing(L0)) {
        L0 <- raster::res(data)[1] * 5
        message(paste0("L0 is set to ", L0, "."))
      }
      if (L0 < 2*raster::res(data)[1]) {
        warning("L0 is smaller than 2*DEM resolution. Consider increasing L0.")
      }

      dem_list <- dem_split(data, L0, parallel = parallel, ncores = ncores)
      if (parallel){
        hs <- parallel::mclapply(dem_list, hr,
                                 mc.cores = ncores) %>% unlist()
      } else{
        hs <- sapply(dem_list, hr)
      }
      rg <- mean(sqrt(((hs^2) / (2 * L0^2)) + 1), na.rm = T)

    } else if (method =="area") {

      if (missing(L0)) {
        L0 <- raster::res(data)[1]
        message(paste0("L0 is set to the resolution of the raster: ", L0, "."))
      }
      if (L0 < raster::res(data)[1]) {
        warning("L0 is smaller than DEM resolution.")
      }
      if (L0 > raster::res(data)[1]) {
        bb <- raster::bbox(data)
        temp <- raster::raster(xmn=bb[1,1], xmx=bb[1,2],
                               ymn=bb[2,1], ymx=bb[2,2], resolution = L0,
                               crs = raster::crs(data))
        r <- terra::project(terra::rast(data), terra::rast(temp))
        r <- raster::raster(r)
        g <- as(r, 'SpatialGridDataFrame')
        sa <- sp::surfaceArea(g, byCell = TRUE)
        sa <- sum(sa@data, na.rm = TRUE)
        rg <- sa/(sum(!is.na(r[]))*L0*L0)

      } else {
        g <- as(data, 'SpatialGridDataFrame')
        sa <- sp::surfaceArea(g, byCell = TRUE)
        sa <- sum(sa@data, na.rm = TRUE)
        rg <- sa/(sum(!is.na(data[]))*L0*L0)
      }
    } else {
      stop("method can only be 'hvar' or 'area'")
    }
  } else if (is(data,  "mesh3d")) {
    res <- Rvcg::vcgMeshres(data)$res[[1]]
    if (missing(L0)){
      L0 <- res
      message(paste("L0 is set to mesh resolution (", L0, ")", sep = ""))
    }
    if (L0 < res) {
      warning("L0 is smaller than mesh resolution")
    }
    if (!L0 == res) {
      mesh <- Rvcg::vcgQEdecim(data, edgeLength = L0, silent = T)
    } else {
      mesh <- data
    }
    rg <- Rvcg::vcgArea(mesh)/planar(mesh, silent = T) # method planar or bbbox
  } else {
    stop("data must be of class RasterLayer or mesh3d with triangular mesh")
  }
  return(rg)
}
