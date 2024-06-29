#' Extract mean depth or elevation of a DEM
#'
#' @param data A DEM in RasterLayer format.
#'
#' @return Value: mean depth or elevation of DEM.
#' @export
#'
#' @examples
#' z(horseshoe)


z <- function(data){
  mean(data[], na.rm = T)
}
