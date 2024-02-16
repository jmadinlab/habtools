#' Calculate sphericity of a 3D object
#'
#' @description Calculates the ratio of the surface area of a sphere with the same volume as the object and the surface area of the object.
#'
#' @param mesh A triangular mesh of class mesh3d.
#'
#' @return Sphericity value.
#' @export
#'
#' @seealso [circularity()]
#'
#' @examples
#' sphericity(mcap)

sphericity <- function(mesh) {
  sa <- suppressWarnings(Rvcg::vcgArea(mesh))
  vol <- suppressWarnings(Rvcg::vcgVolume(mesh))
  ((pi^(1/3))*((6*vol)^(2/3)))/sa
}
