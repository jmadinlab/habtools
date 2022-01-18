#' Transform dem to 3D pointcloud of raster corners.
#'
#' @param dem A digital elevation model in raster format
#'
#' @return A 3D point cloud for raster cell corners
#' @export
#'
#' @examples
dem_to_points <-function(dem){
  res <- (raster::xmax(dem) - raster::xmin(dem))/nrow(dem)
  xmin <- raster::xmin(dem)
  xmax <- raster::xmax(dem)
  ymin <- raster::ymin(dem)
  ymax <- raster::ymax(dem)

  pts <- as.data.frame(raster::rasterToPoints(dem))
  colnames(pts) <- c("x", "y", "z")

  out <- suppressWarnings(lapply(1:nrow(pts), function(i){
    p <- purrr::simplify(pts[i,])
    data.frame(
      x = c(p[1] - res/2, p[1] - res/2, p[1] + res/2, p[1] + res/2),
      y = c(p[2] - res/2, p[2] + res/2, p[2] - res/2, p[2] + res/2),
      z = p[3]
    )
  })) %>% dplyr::bind_rows()

  # add bottom
  add <- rbind(
    unique(out[out$x == xmin, 1:2]),
    unique(out[out$x == xmax, 1:2]),
    unique(out[out$y == ymin, 1:2]),
    unique(out[out$y == ymax, 1:2])
  )
  add$z = 0
  out <- rbind(out, add) %>%
    unique()

  out
}
