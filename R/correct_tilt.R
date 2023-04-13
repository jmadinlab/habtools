#' Correct the tilt of the DEM
#'
#' @param data digital elevation model (DEM)
#'
#' @return DEM with corrected tilt
#' @export
#'
correct_tilt <- function(data){
  x <- seq(data@extent[1]+res(data)[1]/2, data@extent[2]-res(data)[1]/2, res(data)[1])
  y <- seq(data@extent[3]+res(data)[2]/2, data@extent[4]-res(data)[2]/2, res(data)[2])
  z <- values(data)
  xy <- expand.grid(y,x)
  mod <- lm(z ~ xy$Var1 + xy$Var2)
  data@data@values <- mod$residuals
  data
}


