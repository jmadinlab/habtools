#' Entropy
#'
#' Calculates entropy based on heights, 2D, or 3D positions of points.
#'
#' @param data Digital elevation model of class RasterLayer, a triangular mesh of class mesh3d, or a pointcloud of class data.frame.
#' @param ... Additional arguments depending on the data type.
#'
#' @seealso [entropy_1d()]
#' @seealso [entropy_2d()]
#' @seealso [entropy_3d()]
#'
#' @details If data is a 3D pointcloud or 3D mesh, [entropy_3d()] function will be applied;
#' if data is a 2D pointcloud,[entropy_2d()] will be applied; and if data is a RasterLayer, [entropy_1d()] will be applied after conversion to a vector.
#' See the documentation of those functions for details on the necessary arguments.
#'
#' @return A value for entropy.
#' @export
#'
#' @references X. Liu, Q. Ma, X. Wu, T. Hu, Z. Liu, L. Liu, Q. Guo, Y. Su (2022). A novel entropy-based method to quantify forest canopy structural complexity from multiplatform lidar point clouds. Remote Sens. Environ. 282, 113280.
#'
#' @examples
#' entropy(mcap, bw = 0.02, grid_size = 0.01)
#' entropy(horseshoe, grid_size = 0.05)
entropy <- function(data, ...) {
  if(is.vector(data)){
    entropy_1d(data, ...)
  } else if (is(data, "RasterLayer")) {
    entropy_1d(data[], ...)
  } else if (is(data, "mesh3d")) {
    entropy_3d(mesh_to_points(data), ...)
  } else if (dim(data)[2] == 2) {
    entropy_2d(data, ...)
  } else if (dim(data)[2] == 3) {
    entropy_3d(data, ...)
  } else {
    stop("Specified data type is not supported.")
  }
}


#' 3D entropy
#'
#' Calculates 3D entropy
#'
#' @param data data.frame with three columns for x, y, and z coordinates.
#' @param bw Bandwidth to use in 2D kernel density estimator.
#' @param grid_size Size of binning grid, in the unit of the data.
#' @param relative Logical. Rescale entropy relative to the maximum entropy
#' given the number of grid cells? Defaults to FALSE.
#'
#' @details 3D entropy consists of three components, including the projected
#' 2D entropy of the XY plane ($CE_xy$), the projected canopy entropy of the
#' XZ plane ($CE_xz$), and the projected canopy entropy of the
#' YZ plane ($CE_yz$), and the final canopy entropy estimate is calculated
#' as follows: $sqrt(CE_xy^2 + CE_xz^2 + CE_yz^2)$.
#'
#'
#' @return Entropy value.
#' @export
#'
#' @references X. Liu, Q. Ma, X. Wu, T. Hu, Z. Liu, L. Liu, Q. Guo, Y. Su (2022). A novel entropy-based method to quantify forest canopy structural complexity from multiplatform lidar point clouds. Remote Sens. Environ. 282, 113280.
#'
#' @examples
#' dta <- data.frame(x = rnorm(100,5,1), y = rnorm(100,5,1), z = rnorm(100,5,1))
#' entropy_3d(dta, bw = 0.5, 0.25)
#' entropy_3d(dta, bw = 0.5, 0.25, relative = TRUE)
entropy_3d = function(data, bw, grid_size, relative = FALSE){

  en <- sapply(list(data[,c(1,2)], data[,c(1,3)], data[,c(2,3)]),
               entropy_2d, bw = bw, grid_size = grid_size, relative = relative, add_max = TRUE)

  if (relative) {
    ent <- unlist(en[1,])
    ent_max <- unlist(en[2,])
    return(sqrt(sum(ent^2))/sqrt(sum(ent_max^2)))
  }
  else{
    return(sqrt(sum(en^2)))
  }
}


#' 2D Entropy
#'
#' Calculates 2D entropy.
#'
#' @param data data.frame with two columns for x and y coordinates.
#' @param bw Bandwidth to use in 2D kernel density estimator.
#' @param grid_size Size of binning grid, in the unit of the data.
#' @param relative Logical. Rescale entropy relative to the maximum entropy
#' given the number of grid cells? Defaults to FALSE.
#' @param add_max Logical. Add the maximum entropy as a return?
#'
#' @return entropy value
#' @export
#' @references X. Liu, Q. Ma, X. Wu, T. Hu, Z. Liu, L. Liu, Q. Guo, Y. Su (2022). A novel entropy-based method to quantify forest canopy structural complexity from multiplatform lidar point clouds. Remote Sens. Environ. 282, 113280.
#'
#' @examples
#' dta <- data.frame(x = rnorm(100,5,1), y = rnorm(100,5,1))
#' entropy_2d(dta, bw = 0.5, 0.25)
#' entropy_2d(dta, bw = 0.5, 0.25, relative = TRUE)
#' dta <- data.frame(x = runif(100, 1,10), y = runif(100, 1,10))
#' entropy_2d(dta, bw = 0.5, 0.25)
#' entropy_2d(dta, bw = 0.5, 0.25, relative = TRUE)

entropy_2d <- function(data, bw, grid_size, relative = FALSE, add_max = FALSE) {

  xmin <- min(data[,1])
  xmax <- max(data[,1])
  ymin <- min(data[,2])
  ymax <- max(data[,2])

  dx <- grid_size*ceiling((xmax-xmin + 4*bw)/grid_size) - ((xmax-xmin))
  dy <- grid_size*ceiling((ymax-ymin + 4*bw)/grid_size) - ((ymax-ymin))

  xmin <- xmin - dx/2
  xmax <- xmax + dx/2
  ymin <- ymin - dy/2
  ymax <- ymax + dy/2

  nx <- ((xmax-xmin)/grid_size)
  ny <- ((ymax-ymin)/grid_size)

  den = ks::kde(data, h=bw, gridsize = c(nx,ny),
                xmin = c(xmin, ymin), xmax = c(xmax, ymax))
  den = den$estimate[den$estimate > 0]
  den <- den/sum(den)

  en <- -1 * sum(den*log(den))

  den_max <- rep(sum(den)/(nx*ny), each = nx*ny)
  en_max <- -1 * sum(den_max*log(den_max))

  if (relative & add_max) {
    return(list(entropy = en,
                entropy_max = en_max,
                entropy_rel = en/en_max))
  } else if (relative & add_max == FALSE) {
    return(en/en_max)
  } else {
    return(en)
  }
}


#' 1D Entropy
#'
#' Calculates entropy in 1 dimension
#'
#' @param x A numeric vector.
#' @param grid_size Bin size in the same unit as the data.
#' @param relative Logical. Rescale entropy relative to the maximum entropy
#' given the number of grid cells? Defaults to FALSE.
#'
#' @return Entropy value.
#' @export
#'
#' @examples
#' x <- rnorm(1000, 5, 1)
#' entropy_1d(x, grid_size = 0.1)
#' entropy_1d(x, grid_size = 0.1, relative = TRUE)
#' y <- runif(1000, 1, 10)
#' entropy_1d(y, grid_size = 0.1)
#' entropy_1d(y, grid_size = 0.1, relative = TRUE)
entropy_1d <- function(x, grid_size, relative = FALSE) {

  x <- x[!is.na(x)]
  xmin <- min(x)
  xmax <- max(x)

  dx <- grid_size*ceiling((xmax-xmin + 2*grid_size)/grid_size) - ((xmax-xmin))

  xmin <- xmin - dx/2
  xmax <- xmax + dx/2

  nx <- ((xmax-xmin)/grid_size)

  b <- as.integer(cut(x, breaks = seq(xmin, xmax, grid_size)))
  den <- as.numeric(as.data.frame(table(b))$Freq)
  den <- den/sum(den)

  en <- -1 * sum(den*log(den))

  if (relative) {
    den_max <- rep(1/(nx), each = nx)
    en_max <- -1 * sum(den_max*log(den_max))
  }

  if (relative) {
    return(en/en_max)
  } else {
    return(en)
  }
}







