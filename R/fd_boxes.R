#' Calculate fractal dimension using box counting
#'
#' @param data data frame of xy coordinates
#' @param lvec scales to use for calculation (i.e. box sizes)
#' @param keep_data Keep calculation data. Default = FALSE.
#' @param plot Plot number of filled boxes at different scales. Default = FALSE.
#'
#' @details This function calculates fractal dimension using the cube counting method.
#' Based on lvec, cubes of different sizes are defined and the function counts mesh points that fall within each cube.
#' It is recommended that to specify the maximum value of `lvec` so that the largest box encapsulates the entire object.
#' The smallest scale included in `lvec` should not be smaller than the resolution of your object.
#' Most meshes are not perfectly fractal, and so use the `plot` parameter to look for scale transitions.
#'
#' @return A `data.frame` of number of boxes (`n`) intersecting xy coordinates at different box sizes (`l`) and a fractal dimension value. Note that box size is the length of a side.
#' @export
#'
#' @examples
#' mcap_2d <- mesh_to_2d(mcap)
#'
#' fd_boxes(mcap_2d, lvec = c(0.05, 0.1, 0.25, 0.5))
#'

fd_boxes <- function(pts, lvec=NULL, plot = FALSE, keep_data = FALSE) {

  res <- median(perimeter(pts, keep_data = TRUE)$segments)

  if (missing(lvec)) {
    Lmax <- max(diff(apply(pts, 2, range)))
    L0 <- res * 10
    lvec <- seq(L0, Lmax, length.out=25)
  } else {
    L0 <- min(lvec)
    Lmax <- max(lvec)
  }

  # some checks
  if (min(lvec) < res){
    warning("The smallest scale included in lvec is smaller than recommended.")
  }
  if (max(lvec) < Lmax){
    warning("The largest scale included in lvec is smaller than recommended. Consider adjusting to a size that encapsulate the entire mesh.")
  }

  x0 <- min(pts[,1]) - res/2
  y0 <- min(pts[,2]) - res/2

  boxes <- unique(round(Lmax/lvec))
  l <- unique(Lmax/boxes)

  n <- sapply(boxes, function(n) cell_count_2d(pts, x0, x0 + Lmax, y0, y0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(l))
  f <- -as.numeric(coef(mod)[2])

  # plot
  if (plot) {
    plot(pts, asp=1, type="l", axes=FALSE)
    rect(x0, y0, x0 + l, y0 + l, lty=2)
    axis(1)
    axis(2, las=2)
  }
  # output
  if (keep_data) {
    return(list(fd = f, lvec=l, data = data.frame(l = l, n = n)))
  } else {
    return(f)
  }
}

