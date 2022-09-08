## code to prepare `data-raw/horseshoe.tif` dataset goes here

horseshoe <- raster::raster("data-raw/horseshoe.tif")
res(horseshoe)
horseshoe <- raster::aggregate(horseshoe, fact = 5)
res(horseshoe)
usethis::use_data(horseshoe, overwrite = TRUE)

