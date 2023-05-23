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
#' library(raster)
#' dem <- mesh_to_dem(mcap)
#' plot(dem)
#'
#' dem <- mesh_to_dem(mcap, res=0.05)
#' plot(dem)
#'
#' # Don't fill empty raster cells
#' dem <- mesh_to_dem(mcap, res=0.05, fill=FALSE)
#' plot(dem)
#'
mesh_to_dem <- function(mesh, res, fill = TRUE) {
  pts <- data.frame(t(mesh$vb)[,1:3])
  names(pts) <- c("x", "y", "z")
  sp::coordinates(pts) = ~x+y

  if (missing(res)) {
    res <- Rvcg::vcgMeshres(mesh)[[1]]
    res <- max(res)
  }

  rast <- raster::raster(ext=raster::extent(pts), resolution=res)
  rast <- raster::rasterize(pts, rast, pts$z, fun=max)

  if (fill) rast[is.na(rast)] <- min(raster::values(rast), na.rm=TRUE)
  return(rast)
}
