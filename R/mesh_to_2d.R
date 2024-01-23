mesh_to_2d <- function(mesh, L0 = NULL, silent = T){

  res <- Rvcg::vcgMeshres(mesh)$res[[1]]
  if (missing(L0)){
    L0 <- res
    if (!silent){
      message(paste("L0 is set to mesh resolution (", L0, ")", sep = ""))
    }
  }
  if (L0 < res) {
    if (!silent){
      warning("L0 is smaller than mesh resolution")
    }
  }

  if (L0 > res) {
    mesh <- Rvcg::vcgQEdecim(mesh, edgeLength = L0, silent = T)
  }

  # get normals of faces
  m <- mesh
  n <- Rvcg::vcgFaceNormals(mesh)
  # remove faces facing downward
  t <- which(n[3,]<0)
  m$it <- m$it[,-t]
  m$vb[3,] <- 0
  m <- Rvcg::vcgQEdecim(m, edgeLength = L0, silent = T)
  #m <- Rvcg::vcgUniformRemesh(m, voxelSize = L0)
  x <- m$vb[1,]
  y <- m$vb[2,]
  dt <- data.frame(x =x, y = y)
  poly <- concaveman::concaveman(as.matrix(dt), concavity = 1, length_threshold = L0)
  plot(poly)
  polygon(poly)
  return(poly)
}



perimeter <- function(data){
  sum(sapply(1:(nrow(data)-1), function(i){
    dist(data[c(i, i+1),])
  }))
}

circularity <- function(data){
  4*pi*geometry::polyarea(data[,1], data[,2])/(perimeter(data)^2)
}


# planar(mcap)
# mcap_2d <- mesh_to_2d(mcap)
# perimeter(mcap_2d)
# circularity(mcap_2d)


