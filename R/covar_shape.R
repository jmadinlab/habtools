#' Fit covariance-distance curve
#'
#' @param data dataframe with, x, y, and z
#'
#' @return list including rhosq and range
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' data <- expand.grid(x = 1:10, y = 1:10)
#' data$z <- rnorm(100, 5, 1)
#' habtools::covar_shape(data)
#'
covar_shape <- function(data) {

  data <- as.data.frame(data)
  colnames(data) <- c("x", "y", "z")

  data$id <- 1:nrow(data)

  data2 <- data %>%
    dplyr::rename(x2 =x, y2 = y, z2 = z, id2 = id)


  long <- tidyr::expand_grid(id = data$id, id2 = data$id) %>%
    dplyr::left_join(data) %>% dplyr::left_join(data2) %>%
    dplyr::mutate(dist = dist3d(x, y , z, x2, y2, z2))

  maxdist <- max(long$dist)
  cut <- 0.5

  maxcovar <- cov(long[long$dist == 0, "z"], long[long$dist == 0, "z2"])[1,1]

  long <- long %>%
    dplyr::filter(dist < cut*maxdist) %>%
    dplyr::mutate(dist_rounded = round(dist, 3)) %>%
    dplyr::group_by(dist_rounded) %>%
    dplyr::summarise(covar = cov(z, z2))

  range <- min(long[long$covar < 0, "dist_rounded"], na.rm = T)

  sub <- long %>%
    dplyr::filter(dist_rounded < range)

  sub[sub$dist_rounded == 0, "covar"] <- maxcovar

  mod <- nls(covar ~ (sigmasq*(exp(-rhosq * (dist_rounded^2)))),
             start = list(rhosq = 0.1, sigmasq = 2),
             data = sub) %>%
    summary()
  rhosq <- mod$coefficients[1,1]
  sigmasq <- mod$coefficients[2,1]

  ggplot(sub) +
    geom_point(aes(x = dist_rounded, y = covar)) +
    geom_line(aes(x = dist_rounded,
                  y = sigmasq*(exp(-rhosq * dist_rounded^2)))) +
    labs(x = "Distance", y = "Covariance")

  return(list(rhosq = rhosq, range = range))
}

dist3d <- function(x1, y1, z1, x2, y2, z2){
  ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)^(0.5)
}

