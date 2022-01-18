## code to prepare `data-raw/horseshoe.tif` dataset goes here

horseshoe <- raster::raster("data-raw/horseshoe.tif")
usethis::use_data(horseshoe, overwrite = TRUE)
