#' Colony Shape Factor
#'
#' @description Calculates mechanical vulnerability of rigid, cantilever-type
#' structural elements.
#'
#' @param mesh A mesh3d object
#' @param z_min (optional) set the z plane about which csf should be calculated
#' @param res (optional) set the resolution
#'
#' @details This function calculates the mechanical vulnerability of a structural
#' element, like a hard coral colony, to fluid flow. While developed for corals, and originally
#' called the Colony Shape Factor (CSF), the function is applicable to any attached,
#' rigid cantilever type structure. CSF is dimensionless and can be used to compare the
#' vulnerability among structures. Mechanistically, if the CSF of a structure becomes greater
#' than the dislodgement mechanical threshold, breakage occurs. This threshold is a function of
#' material tensile strength and inversely related to fluid velocity and density (Madin & Connolly 2006).
#'
#' @note The orientation of the 3D mesh is important for this function. The function assumes the fluid flow is parallel with the y-axis. The function also assumes the base of the cantilever over which the bending moment acts can be approximated as an ellipse with the diameter on the y-axis parallel with flow (dy). You can set a z_min if the base of your mesh is not flat at the base (i.e., shift the plane upon which the cantilever is attached upwards). The function output includes dy and dx for monitoring anticipated values.
#'
#' @return A list containing the colony shape factor (csf), the parallel to flow (dy) and perpendicular (dx) diameters of the cantilever base, and the bending moment (mom).
#' @export
#'
#' @references Madin JS & Connolly SR (2006) Ecological consequences of major hydrodynamic disturbances on coral reefs. Nature. 444:477-480.
#'
#' @examples
#' csf(mcap, z_min=-3.65)

csf <- function(mesh, z_min, res) {
  pts <- data.frame(t(mesh$vb)[,1:3])
  names(pts) <- c("x", "y", "z")

  if (missing(z_min)) {
    z_min <- min(pts$z)
    warning(paste0("z_min set to ", z_min))
  }
  if (z_min < min(pts$z) | z_min > max(pts$z)) {
    stop("z_min outside the range of z values")
  }
  if (missing(res)) {
    res <- Rvcg::vcgMeshres(mesh)$res
    res <- max(res)
    warning(paste0("resolution set to ", res))
  }

  pts <- pts[pts$z >= z_min,]
  base <- pts[pts$z <= (z_min + res),]
  dy <- diff(range(base$y)) # Diameter parallel to flow
  dx <- diff(range(base$x)) # Diameter perpendicular to flow

  sp::coordinates(pts) = ~x+z
  rast <- raster::raster(ext=raster::extent(pts), resolution=res)
  rast <- raster::rasterize(pts, rast, pts$y, fun=max)
  raster::values(rast)[!is.na(raster::values(rast))] <- 1

  moment <- function (i) {
    y <- raster::yFromRow(rast, i)
    (y - z_min) * sum(raster::values(rast)[raster::coordinates(rast)[,2] == y], na.rm=TRUE) * res^2
  }

  mom <- sum(sapply(1:dim(rast)[1], moment))
  csf <- (16 / (dy^2 * dx * pi)) * mom

  return(list(csf=csf, dy=dy, dx=dx, mom=mom))
}
