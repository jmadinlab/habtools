#' Calculate fractal dimension using the box counting method
#'
#' @param data A data frame in which the first two columns are x and y coordinates, respectively.
#' @param lvec The scales to use for calculation (i.e. box sizes).
#' @param keep_data Logical. Keep calculation data? Default = TRUE.
#' @param plot Logical. Plot the shape with box sizes superimposed? Defaults to FALSE.
#'
#' @details This function calculates fractal dimension using the box counting method.
#' If `lvec` is not specified, a default based on resolution and extent will be used.
#' Based on lvec, boxes of different sizes are defined and the function counts boxes that capture the outline of the shape.
#' It is recommended to specify the maximum value of `lvec` so that the largest
#' box encapsulates the entire object. The smallest scale included in `lvec`
#' should not be smaller than the resolution of your object.
#'
#' @return A value for fractal dimension, typically between 1 and 2 or a list if keep_data = TRUE.
#' @export
#'
#' @importFrom graphics axis
#'
#' @examples
#' mcap_2d <- mesh_to_2d(mcap)
#'
#' fd_boxes(mcap_2d, plot = TRUE, keep_data = TRUE)
#' fd_boxes(mcap_2d, lvec = c(0.05, 0.1, 0.2, 0.4), plot = TRUE)
#'

fd_boxes <- function(data, lvec, keep_data = FALSE, plot = FALSE) {

  pts <- data
  res <- quantile(perimeter(pts, keep_data = TRUE)$segments, 0.75)
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
    message("The smallest scale included in lvec is smaller than the resolution. Consider adjusting lvec. ")
  }
  if (max(lvec) < Lmax){
    message("The largest scale included in lvec is smaller than the size of the object.")
  }

  x0 <- min(data[,1]) - res/2
  y0 <- min(data[,2]) - res/2

  n <- sapply(boxes, function(n) cell_count_2d(data, x0, x0 + Lmax, y0, y0 + Lmax, n))

  mod <- lm(log10(n) ~ log10(lvec))
  fd <- -as.numeric(coef(mod)[2])

  # plot
  if (plot) {
    plot(data, asp = 1, type = "l", axes = FALSE,
         xlim = c(x0, x0 + Lmax), ylim = c(y0, y0 + Lmax))
    rect(x0, y0, x0 + lvec, y0 + lvec, border = "red")
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

