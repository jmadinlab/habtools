#' DEM to 3D mesh
#'
#' @param dem A digital elevation model in raster format
#'
#' @return A 3D points cloud for raster cell corners
#' @export
#' @description This function is very slow and needs work.
#'
#' @examples
#' mesh <- dem_to_mesh(horseshoe)
#' # library(rgl)
#' plot3d(mesh)
#'
dem_to_mesh <- function(dem) {
  pts <- as.data.frame(rasterToPoints(dem))
  names(pts) <- c("x", "y", "z")
  res <- res(dem)

  corns <- data.frame()
  for (i in 1:nrow(pts)) {
    corns <- rbind(corns, data.frame(x=pts$x[i]+res[1]/2, y=pts$y[i]+res[2]/2, z=pts$z[i]))
    corns <- rbind(corns, data.frame(x=pts$x[i]+res[1]/2, y=pts$y[i]-res[2]/2, z=pts$z[i]))
    corns <- rbind(corns, data.frame(x=pts$x[i]-res[1]/2, y=pts$y[i]+res[2]/2, z=pts$z[i]))
    corns <- rbind(corns, data.frame(x=pts$x[i]-res[1]/2, y=pts$y[i]-res[2]/2, z=pts$z[i]))
  }
  corns <- round(corns, 6)
  corns <- corns[!duplicated(corns),]

  pts <- data.frame()
  while (nrow(corns) > 0) {
    temp <- corns[corns$x == corns$x[1] & corns$y == corns$y[1],]
    corns <- corns[!(corns$x == corns$x[1] & corns$y == corns$y[1]),]
    pts <- rbind(pts, data.frame(x=temp$x[1], y=temp$y[1], z=c(seq(min(temp$z), max(temp$z), min(res)), max(temp$z))))
  }
  pts <- pts[!duplicated(pts),]

  return(pts)
}

# plot3d(pts)
