#' Calculate fractal dimension using box counting
#'
#' @param data A data frame in which the first two colums and x and y coordinates, respectively.
#' @param lvec (Optional). The scales to use for calculation (i.e. box sizes).
#' @param keep_data Logical. Keep calculation data? Default = TRUE.
#' @param plot Logical. Plot the shape with box sizes superimposed? Default = FALSE.
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
#' @importFrom graphics axis
#'
#' @examples
#' mcap_2d <- mesh_to_2d(mcap)
#'
#' fd_boxes(mcap_2d, plot=TRUE, keep_data=TRUE)
#' fd_boxes(mcap_2d, lvec = c(0.05, 0.1, 0.25, 0.5), plot=TRUE)
#'

fd_boxes <- function(data, lvec=NULL, keep_data = TRUE, plot = FALSE) {

  pts <- data
  res <- median(perimeter(pts, keep_data = TRUE)$segments)
  names(pts) <- c("x", "y")

  if (missing(lvec)) {
    Lmax <- max(diff(apply(pts, 2, range))) + res
    L0 <- res
    boxes <- 2^(0:20)
    lvec <- Lmax / boxes
    boxes <- boxes[lvec > L0]
    lvec <- lvec[lvec > L0]
  } else {
    L0 <- min(lvec)
    Lmax <- max(lvec)
    boxes <- unique(round(Lmax/lvec))
    lvec <- unique(Lmax/boxes)
  }

  # some checks
  if (min(lvec) < res){
    warning("The smallest scale included in lvec is smaller than recommended.")
  }
  if (max(lvec) < Lmax){
    warning("The largest scale included in lvec is smaller than recommended. Consider adjusting to a size that encapsulate the entire mesh.")
  }

  x0 <- min(data[,1]) - res/2
  y0 <- min(data[,2]) - res/2

  n <- sapply(boxes, function(n) cell_count_2d(data, x0, x0 + Lmax, y0, y0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(lvec))
  fd <- -as.numeric(coef(mod)[2])

  # plot
  if (plot) {
    plot(data, asp=1, type="l", axes=FALSE, xlim=c(x0, x0 + Lmax), ylim=c(y0, y0 + Lmax))
    rect(x0, y0, x0 + lvec, y0 + lvec, border="red")
    axis(1)
    axis(2, las=2)
  }
  # output
  if (keep_data) {
    return(list(D = unname(fd), lvec = lvec, data = data.frame(l = lvec, n = n), method = "boxes"))
  } else {
    return(unname(fd))
  }
}

