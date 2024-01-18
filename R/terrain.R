#' Simulates a fractal surface (terrain)
#'
#' Simulates z-values based on the Diamond-square algorithm.
#' Warning: this function gets slow for n > 128.
#'
#' @param n number of rows or cols
#' @param smoothness a value between 0.0 and 1.0 (lower values
#' produce rougher terrain)
#' @param z_extent scale z to match extent (default FALSE)
#' @param dem TRUE (default) or FALSE
#'
#' @return a matrix or raster (if dem = TRUE)
#' @export
#'
#' @examples
#' library(raster)
#' dem <- habtools::terrain(32, 0)
#' plot(dem)

terrain <- function(n, smoothness, z_extent=FALSE, dem=TRUE) {
  n_ <- smallestPowerOfTwoAfter(n)

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

  mat <- mat[1:n, 1:n]
  mat <- mat - min(mat)
  if (z_extent) { mat <- mat * (n / diff(range(mat))) }

  if (dem) {
    mat <- raster::raster(mat, xmn=0, xmx=n, ymn=0, ymx=n)
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

  if (runif(1) > 0.5) {sign <- 1} else {sign <- -1}
  reduce <- 1
  for (i in 1:depth) {
    reduce <- reduce * 2^(-smoothness)
  }
  return(sign * runif(1) * reduce)
}
