#' Calculate sphericity
#'
#' @param mesh mesh 3d object
#'
#' @details The ratio of the surface area of a sphere with the same volume as the object and the surface area of the object
#'
#' @return value
#' @export
#'
#' @examples
#' sphericity(mcap)

sphericity <- function(mesh) {
  sa <- suppressWarnings(Rvcg::vcgArea(mesh))
  vol <- suppressWarnings(Rvcg::vcgVolume(mesh))
  ((pi^(1/3))*((6*vol)^(2/3)))/sa
}
