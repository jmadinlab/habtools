#' DEM to 3D mesh
#'
#' @param dem A digital elevation model in raster format
#'
#' @return A 3D points cloud for raster cell corners
#' @export
#' @description This function is very slow and needs work.
#'
#' @examples
#' raster <- raster(nrows = 10, ncols = 10, vals = rnorm(100, -10, 5))
#' mesh <- habtools::dem_to_mesh(raster)



dem_to_mesh <- function(dem){

  # get points of all corners
  p1 <- dem_to_points(dem)


  xmin <- min(p1$x)
  xmax <- max(p1$x)
  ymin <- min(p1$y)
  ymax <- max(p1$y)

  zmin <- min(p1$z)

  res <- res(dem)
  res_x <- res[1]
  res_y <- res[2]

  # add index
  p1$index <- 1:nrow(p1)

  ### get all faces ###
  # bottom
  b <- p1[p1$z == zmin,]
  i_b <-
    c(b[b$x == min(b$x) & b$y == min(b$y), "index"],
      b[b$x == min(b$x) & b$y == max(b$y), "index"],
      b[b$x == max(b$x) & b$y == max(b$y), "index"],
      b[b$x == max(b$x) & b$y == min(b$y), "index"]) %>%
    as.matrix(nrow = 4)

  # top
  t <- p1[!abs(p1$z - min(p1$z))<0.1*res_x,]
  topgr <- expand.grid(x = seq(xmin, (xmax - res_x), by = res_x),
                       y = seq(ymin, (ymax - res_y), by = res_y))
  i_t <- sapply(1:nrow(topgr), function(i){
    px <- topgr[i,"x"]
    py <- topgr[i,"y"]

    pz <- dplyr::intersect((t[abs(t$x - px)<(0.1*res_x)
                              & abs(t$y - py)<(0.1*res_y), "z"]),
                           (t[abs(t$x - (px + res_x))<(0.1*res_x) &
                                abs(t$y - (py +res_y))<(0.1*res_y), "z"]))

    c(t[abs(t$x - px)<(0.1*res_x)  & abs(t$y - py)<(0.1*res_y)  & t$z == pz , "index"],
      t[abs(t$x - (px + res_x))<(0.1*res_x) & abs(t$y - py)<(0.1*res_y)  & t$z == pz, "index"],
      t[abs(t$x - (px + res_x))<(0.1*res_x) & abs(t$y - (py + res_y))<(0.1*res_y) & t$z == pz, "index"],
      t[abs(t$x - px)<(0.1*res_x)  & abs(t$y - (py + res_y))<(0.1*res_y) & t$z == pz, "index"])
  })

  # sides
  gr <- expand.grid(x = seq(xmin, (xmax - res_x), by = res_x),
                    y = seq(ymin, (ymax - res_y), by = res_y))

  i_s1 <- sapply(1:nrow(gr), function(i){
    px <- gr[i,"x"]
    py <- gr[i,"y"]
    z <- dplyr::intersect((p1[abs(p1$x - px) < (0.1 * res_x) &
                                abs(p1$y - py) < (0.1 * res_y), "z"]),
                          (p1[abs(p1$x - px) < (0.1 * res_x) &
                                abs(p1$y - (py + res_y)) < (0.1 * res_y), "z"]))
    zmin <- min(z)
    zmax <- max(z)

    c(p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & p1$z == zmin, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & p1$z == zmax, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - (py + res_y)) < (0.1 * res_y) & p1$z == zmax, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - (py + res_y)) < (0.1 * res_y) & p1$z == zmin, "index"]
    )
  })

  i_s2 <- sapply(1:nrow(gr), function(i){
    px <- gr[i,"x"]
    py <- gr[i,"y"]
    z <- dplyr::intersect((p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y), "z"]),
                          (p1[abs(p1$x - (px  + res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y), "z"]))
    zmin <- min(z)
    zmax <- max(z)

    c(p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"],
      p1[abs(p1$x - (px + res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"],
      p1[abs(p1$x - (px + res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"]
    )
  })

  ### add outer sides
  gr <- expand.grid(x = xmax,
                    y = seq(ymin + res_y, ymax, by = res_y))
  i_s3 <- sapply(1:nrow(gr), function(i){
    px <- gr[i,"x"]
    py <- gr[i,"y"]
    z <- dplyr::intersect((p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y), "z"]),
                          (p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - (py - res_y)) < (0.1 * res_y), "z"]))
    zmin <- min(z)
    zmax <- max(z)

    c(p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - (py - res_y)) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - (py - res_y)) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"]
    )
  })

  gr <- expand.grid(x = seq(xmin + res_x, xmax, by = res_x),
                    y = ymax)
  i_s4 <- sapply(1:nrow(gr), function(i){
    px <- gr[i,"x"]
    py <- gr[i,"y"]
    z <- dplyr::intersect((p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y), "z"]),
                          (p1[abs(p1$x - (px - res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y), "z"]))
    zmin <- min(z)
    zmax <- max(z)

    c(p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"],
      p1[abs(p1$x - (px - res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmin) < 0.00001, "index"],
      p1[abs(p1$x - (px - res_x)) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"],
      p1[abs(p1$x - px) < (0.1 * res_x) & abs(p1$y - py) < (0.1 * res_y) & abs(p1$z - zmax) < 0.00001, "index"]
    )
  })

  index <- cbind(i_b, i_t, i_s1, i_s2, i_s3, i_s4)

  mesh1 <- rgl::mesh3d(x = p1$x, y = p1$y, z = p1$z, quads = index)

  rgl::plot3d(mesh1, alpha = 0.5)

  return(mesh1)

}
