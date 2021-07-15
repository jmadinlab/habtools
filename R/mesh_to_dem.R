#' 3D Mesh to DEM
#'
#' @description
#' `mesh_to_dem` turns a 3D Mesh file into a Digital Elevation Model (DEM) in raster
#' format.
#'
#' @param mesh A mesh3d object.
#' @param res (Optional) The desired DEM resolution in same units at the 3D mesh.
#' @param fill Fill `NA` values in raster with minimum value.
#'
#' @return A raster file.
#' @export
#'
#' @details
#' The function rasterizes uses the vertices of the mesh file.
#' If resolution is not
#' given, it is calculated by finding the maximum nearest neighbor
#' of vertices projected
#' on the `xy` plane. `fill` is used when irregular 3D meshes
#' result in `NA` values in
#' raster cells. The default is to fill these cells with the
#' minimum, non-`NA` raster value.
#'
#' @examples
#' dem <- mesh_to_dem(mcap)
#' plot(dem)
#' rg(dem, -1.35, 0.85, 0.2, 0.05, plot=TRUE)
#'
#' dem <- mesh_to_dem(mcap, res=0.05)
#' plot(dem)
#'
#' # Don't fill empty raster cells
#' dem <- mesh_to_dem(mcap, res=0.05, fill=FALSE)
#' plot(dem)
#'
mesh_to_dem <- function(mesh, res, fill=TRUE) {
  pts <- data.frame(t(mesh$vb)[,1:3])
  names(pts) <- c("x", "y", "z")
  sp::coordinates(pts) = ~x+y

  if (missing(res)) {
    dts <- as.matrix(dist(sp::coordinates(pts), diag=FALSE, upper=TRUE))
    dts[dts==0] <- NA
    res <- apply(dts, 1, min, na.rm=TRUE)
    res <- max(res)*sqrt(2)
  }

  rast <- raster::raster(ext=raster::extent(pts), resolution=res)
  rast <- raster::rasterize(pts, rast, pts$z, fun=max)

  if (fill) rast[is.na(rast)] <- min(raster::values(rast), na.rm=TRUE)
  return(rast)
}
