### Complexity project ##

# Libriaries
library("devtools")
library("raster")
library("geomorph")
library("Rvcg")

# Functions

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


hvar <- function(dem, x, y, L, Lvec) {
  hvar <- data.frame()
  for (L0 in Lvec) {
    cells <- expand.grid(x=seq(from = x, to = x+L-L0, by = L0), y=seq(y, y+L-L0, L0))
    H0 <- mapply(hr, x=cells$x, y=cells$y, MoreArgs=list(L=L0, data=dem))
    hvar <- rbind(hvar, data.frame(L0=L0, H0=H0))
  }
  return(hvar)
}


# Data

in_dr <- "data"
files <- list.files(path = in_dr, full.names = T)
file_x <- files[6]
ply_x <- vcgPlyRead(file = file_x)

dem_x <- mesh_to_dem(mesh = ply_x, res=0.001)
plot(dem_x)

hr(dem_x)
rg(dem_x, L0=0.01, plot=TRUE)

data <- hvar(dem_x, Lvec=0.15/c(1, 2, 4, 8, 16, 32, 64))
fd(data, plot=TRUE, method = "median")
fd(data, plot=TRUE, method = "mean")
fd(data, plot=TRUE, method = "ends")


x1 <- dem_x@extent@xmin
y1 <-  dem_x@extent@ymin
L1_x <-  dem_x@extent@xmax - dem_x@extent@xmin
L1_y <-  dem_x@extent@ymax - dem_x@extent@ymin


L1_x == L1_y

hvar_x <- hvar(dem = dem_x, x = x1, y = y1, L = L1_x, Lvec = c(2, 1, 0.5))
