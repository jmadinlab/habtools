## code to prepare `data-raw/horseshoe.tif` dataset goes here

horseshoe <- raster::raster("data-raw/horseshoe.tif")
res(horseshoe)
bb <- raster::bbox(data)
temp <- raster::raster(xmn=bb[1,1], xmx=bb[1,2],
                       ymn=bb[2,1], ymx=bb[2,2], resolution = 0.01,
                       crs = raster::crs(data))
r <- terra::project(terra::rast(data), terra::rast(temp))
horseshoe <- raster::raster(r)
res(horseshoe)
usethis::use_data(horseshoe, overwrite = TRUE)

