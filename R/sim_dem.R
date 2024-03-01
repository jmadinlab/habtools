#' Simulates a fractal DEM
#'
#' Simulates z-values based on the Diamond-square algorithm.
#'
#' @param L The extent.
#' @param smoothness A value between 0.0 and 1.0 (lower values
#' produce rougher DEM).
#' @param H Desired height range (optional).
#' @param R Desired rugosity value (optional).
#' @param method The method to be used for rugosity calculation in case R is given. Can be "hvar" or "area"
#' @param parallel Logical. Use parallel processing? Defaults to FALSE. Only relevant if method = "hvar".
#' @param n Number of iterations to try and reach desired R. Recommended to adapt R and H instead of increasing n if simulation fails.
#' @param prop Proportion of cells that undergo smoothing at each iteration when R is provided.
#' @param plot Logical. Plot the simulated DEM during simulation? Only relevant if R is provided.
#'
#' @importFrom grDevices dev.flush
#'
#' @return  Digital elevation model of class RasterLayer.
#'
#' @details
#' Warning: this function gets slow for n > 128.
#' If H is provided, the simulated DEM is rescaled based on the value for H.
#' If R is provided, a DEM is simulated using the same algorithm based on R, H, and the predicted D based on [rdh_theory()], while smoothness is ignored.
#' From that first simulated DEM, R is calculated and the DEM undergoes smoothing at each iteration until the rugosity approximates the inputted R.
#' Argument prop defined the proportion of random cells of the DEM that are smoothed by averaging the z values of cell and neighboring cells at each iteration.
#' Caution: When R is provided, the DEM may become increasingly less fractal as it is modified at each iteration.
#'
#' @export
#'
#' @examples
#' library(raster)
#' dem <- sim_dem(L = 32, smoothness = 0.5)
#' plot(dem)
#' dem <- sim_dem(L = 32, smoothness = 0.2, H = 20)
#' plot(dem)

sim_dem <- function(L, smoothness, H, R, plot = FALSE, prop = 0.1,
                    n = 100, method = "area", parallel = FALSE) {

  if(!missing(R) & !missing(H)) {
    R_min <- (0.5*L*sqrt(2*H^2 + 4*L^2))/(L^2)
    if (R < R_min) {
      stop("The chosen R is too low for the specified L and H. Increase R.")
    }
    d <- round(rdh_theory(R = R, H = H, L0 = 1, L = L)[[1]], 1)
    if(d < 2.91) {d <- d + 0.1}
    smoothness <- 3 - d
    if (!missing(smoothness)) {message("smoothness is ignored when R and H are given.")}

    for(i in 1:10) {
      r <- sim_diamond(L = L, smoothness = smoothness)
      r@data@values <- r@data@values * H/hr(r)
      r@data@values <- r@data@values - min(r@data@values)

      # check rugosity
      if (method == "area") {
        ru <- rg(r, L0 = 1, method = "area")
        message(ru)
      } else if (method == "hvar") {
        ru <- rg(r, L0 = 5, method = "hvar", parallel = parallel)
        message(ru)
      }
      if (R < round(ru, 1)) {
        break
      }
    }

    if (R > round(ru, 1)) {
      stop("Run for more iterations or change parameters")
    }

    if (plot) {
      plot(r, legend=FALSE, asp=NA)
    }

    for (i in 1:n) {
      if (R == round(ru, 1)) {
        break
      }

      K <- sample(seq(2,(L-1)), size = L*sqrt(prop))
      J <- sample(seq(2,L-1), size = L*sqrt(prop))
      for (m in 1:length(K)) {
        k <- K[m]
        j <- J[m]
        r[k,j] <- suppressWarnings(
          mean(r[c(k-1, k, k+1), c((j-1), j, j+1)], na.rm = TRUE)
        )
      }
      if (plot) {
        plot(r, legend=FALSE, asp=NA, main=i)
      }
      dev.flush()
      r@data@values[is.na(r@data@values)] <- mean(r@data@values, na.rm = TRUE)
      r@data@values <- r@data@values * H/hr(r)
      r@data@values <- r@data@values - min(r@data@values)

      if (method == "area") {
        ru <- rg(r, L0 = 1, method = "area")
        message(ru)
      } else if (method == "hvar") {
        ru <- rg(r, L0 = 5, method = "hvar", parallel = TRUE)
        message(ru)
      }

    }
    if(!R == round(ru, 1)) {
      stop("Run for more iterations or change parameters")
    }
  } else if (!missing(smoothness)) {
    r <- sim_diamond(L = L, smoothness = smoothness)
    if (!missing(H)) {r <- r * (H / hr(r)) }
  } else {
    stop("Either smoothness or R and H have to be provided.")
  }

  return(r)

}



sim_diamond <- function(L, smoothness, dem = TRUE) {

  n_ <- smallestPowerOfTwoAfter(L)

  depth <-  1
  mat <- matrix(0, n_ + 1, n_ + 1)
  mat[1, 1] <- scaleC(smoothness, depth) # Corners
  mat[1, n_ + 1] <- scaleC(smoothness, depth)
  mat[n_ + 1, 1] <- scaleC(smoothness, depth)
  mat[n_ + 1, n_ + 1] <- scaleC(smoothness, depth)

  numIteration <- log(dim(mat)[1] - 1) / log(2)
  while (depth <= numIteration) {
    mat <- diamond(mat, depth, smoothness)
    mat <- square(mat, depth, smoothness)
    depth <- depth + 1
  }

  mat <- mat[1:L, 1:L]
  mat <- mat - min(mat)

  if (dem) {
    mat <- raster::raster(mat, xmn=0, xmx=L, ymn=0, ymx=L,
                          crs = "+proj=tmerc +datum=WGS84 +units=m +no_defs")


  }

  return(mat)
}

# Supporting functions

smallestPowerOfTwoAfter <- function(n) {
  ret <- 1
  while (ret < n) {
    ret <- bitwShiftL(ret, 1)
  }
  return(ret)
}

diamond <- function(mat, depth, smoothness) {

  len <- dim(mat)[1]
  terrainSize <- len - 1
  numSegs <- bitwShiftL(1, (depth - 1))
  span <- terrainSize / numSegs
  half <- span / 2

  for (x in seq(1, len - span, span)) {
    for (y in seq(1, len - span, span)) {
      # //  (x, y)
      # //    \
      # //     a---b---c
      # //     |   |   |
      # //     d---e---f
      # //     |   |   |
      # //     g---h---i
      # //
      # //     \___ ___/
      # //         V
      # //       span

      va <- c(x, y)
      vc <- c(x + span, y)
      ve <- c(x + half, y + half)
      vg <- c(x, y + span)
      vi <- c(x + span, y + span)

      heights <- mat[rbind(va, vc, vg, vi)]
      avg <- mean(heights)

      offset <- scaleC(smoothness, depth)
      mat[ve[1], ve[2]] <- avg + offset
    }
  }
  return(mat)
}

square <- function(mat, depth, smoothness) {

  len <- dim(mat)[1]
  terrainSize <- len - 1
  numSegs <- bitwShiftL(1, (depth - 1))
  span <- terrainSize / numSegs
  half <- span / 2

  for (x in seq(1, len - span, span)) {
    for (y in seq(1, len - span, span)) {

      # // for each sub-square, the height of the center is caculated
      # // by averaging the height of its four vertices plus a random offset.
      # // for example,
      # //       h = avg(g, c, i, m) + random;
      # //       f = avg(a, g, k, i) + random;
      # //       j = f;
      # //
      # //  (x, y)
      # //    \
      # //     a---b---c---d---e
      # //     | \ | / | \ | / |
      # //     f---g---h---i---j
      # //     | / | \ | / | \ |
      # //     k---l---m---n---o
      # //     | \ | / | \ | / |
      # //     p---q---r---s---t
      # //     | / | \ | / | \ |
      # //     u---v---w---x---y
      # //
      # //     \___ ___/
      # //         V
      # //       span

      va <- c(x, y)
      vb <- c(x + half, y)
      vc <- c(x + span, y)
      vf <- c(x, y + half)
      vg <- c(x + half, y + half)
      vh <- c(x + span, y + half)
      vk <- c(x, y + span)
      vl <- c(x + half, y + span)
      vm <- c(x + span, y + span)

      # // right of h
      vhr <- c(x + half * 3, y + half)
      if (vhr[1] > terrainSize + 1) vhr[1] <- vhr[1] - half

      # // left of f
      vfl <- c(x - half, y + half)
      if (vfl[1] < 1) vfl[1] <- terrainSize - half

      # // under l
      vlu <- c(x + half, y + half * 3)
      if (vlu[2] > terrainSize + 1) vlu[2] <- vlu[2] - half

      # // above b
      vba <- c(x + half, y - half)
      if (vba[2] < 1) vba[2] <- terrainSize - half

      mat <- squareHelper(mat, depth, smoothness, va, vg, vk, vfl, vf)
      mat <- squareHelper(mat, depth, smoothness, va, vba, vc, vg, vb)
      mat <- squareHelper(mat, depth, smoothness, vc, vhr, vm, vg, vh)
      mat <- squareHelper(mat, depth, smoothness, vk, vg, vm, vlu, vl)
    }
  }

  # // set the elevations of the rightmost and bottom vertices to
  # // equal the leftmost and topmost ones'.
  # for (y in seq(1, len - span, span)) {
  #   mat[y, terrainSize - 1] <- mat[y, 1]
  # }
  # for (x in seq(1, len - span, span)) {
  #   mat[terrainSize - 1, x] <- mat[1, x]
  # }

  # for (var y = 0; y < terrainSize; y += span) {
  #   matrix[y][terrainSize] = matrix[y][0];
  # }
  # for (var x = 0; x < terrainSize; x += span) {
  #   matrix[terrainSize][x] = matrix[0][x];
  # }

  return(mat)
}

squareHelper <- function(mat, depth, smoothness, a, b, c, d, t) {
  heights <- mat[rbind(a, b, c, d)]
  avg <- mean(heights)
  offset <- scaleC(smoothness, depth)
  mat[t[1], t[2]] <- avg + offset
  return(mat)
}

scaleC <- function(smoothness, depth) {

  if (stats::runif(1) > 0.5) {sign <- 1} else {sign <- -1}
  reduce <- 1
  for (i in 1:depth) {
    reduce <- reduce * 2^(-smoothness)
  }
  return(sign * stats::runif(1) * reduce)
}
