#' Transform DEM to 3D pointcloud of raster corners.
#'
#' @param dem Digital elevation model of class RasterLayer.
#' @param bh Border height from lowest point.
#' @param parallel Logical. Use parallel computation?
#'
#' @return A 3D point cloud for raster cell corners.
#'
#' @examples
#' dem <- sim_terrain(20, 0.5)
#' raster::plot(dem)
#' pts <- dem_to_points(dem)
#' rgl::plot3d(pts)
#'
#'

dem_to_points <-function(dem, bh = NULL, parallel = F){

  res <- res(dem)
  res_x <- res[1]
  res_y <- res[2]

  if (is.null(bh)) {
    bh <- res_x
  }

  pts <- as.data.frame(raster::rasterToPoints(dem))
  colnames(pts) <- c("x", "y", "z")

  out <- suppressWarnings(lapply(1:nrow(pts), function(i){
    p <- purrr::simplify(pts[i,])
    data.frame(
      x = c(p[1] - res_x/2, p[1] - res_x/2, p[1] + res_x/2, p[1] + res_x/2),
      y = c(p[2] - res_y/2, p[2] + res_y/2, p[2] - res_y/2, p[2] + res_y/2),
      z = p[3]
    )
  })) %>% dplyr::bind_rows()

  # add bottom
  add <- rbind(
    unique(out[out$x == min(out$x), 1:2]),
    unique(out[out$x == max(out$x), 1:2]),
    unique(out[out$y == min(out$y), 1:2]),
    unique(out[out$y == max(out$y), 1:2])
  )
  add$z <- min(out$z) - bh
  out <- rbind(out, add) %>%
    unique()

  # remove duplicates
  out_r <- out
  rownames(out_r) <- 1:nrow(out)
  out_r <- out_r %>%
    dplyr::mutate_if(is.numeric, round, 5) %>%
    unique()
  out <- out[as.integer(rownames(out_r)),]

  out
}




