#' Calculates mechanical vulnerability of cantilever type structure
#'
#' @param mesh mesh object
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
#' @return A list containing the colony shape factor, the parallel (d1) and perpendicular (d2) diameters of the attachment point, and the second moment of area relative to flow.
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
  d1 <- diff(range(base$y)) # Diameter parallel to flow
  d2 <- diff(range(base$x)) # Diameter perpendicular to flow

  sp::coordinates(pts) = ~x+z
  rast <- raster::raster(ext=raster::extent(pts), resolution=res)
  rast <- raster::rasterize(pts, rast, pts$y, fun=max)
  values(rast)[!is.na(values(rast))] <- 1

  sma <- function (i) {
    y <- yFromRow(rast, i)
    (y - z_min) * sum(values(rast)[coordinates(rast)[,2] == y], na.rm=TRUE) * res^2
  }

  sma <- sum(sapply(1:dim(rast)[1], sma))
  csf <- (16 / (d1^2 * d2 * pi)) * sma

  return(list(csf=csf, d1=d1, d2=d2, sma=sma))
}
