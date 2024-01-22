#' Investigates varying fractal dimensions across scale
#'
#' @param data Digital elevation model of class RasterLayer
#' @param lvec scales to use for calculation
#' @param method Possible methods are:"hvar", "area", and "sd".
#' @param ... Additional arguments of see [fd()]
#' @return A list with fractal dimension across scales, mean fractal dimension, and sd of fractal dimensions across scales.
#' @export
#'
#' @examples
#' fd_diagnose(horseshoe, lvec = c(0.05, 0.1, 0.2, 0.4), method = "area")
#'
fd_diagnose <- function(data, lvec, method, ...) {

  lvec <- sort(lvec)
  d <- fd(data = data, lvec = lvec, method = method, keep_data = TRUE, ...)

  dta <- d[["data"]]
  dval <- d[["fd"]]

  fdvec <- diff(log10(dta$h)) / diff(log10(dta$l))
  if (method == "area") {
    f <- 2 - f
  }
  if (method == "cubes") {
    f <- -f
  }
  if (method == "hvar") {
    f <- 3 - f
  }

  df <- data.frame(l = dta[,1][1:length(dta[,1])-1], fd = f)

  plot(dta[,2] ~ dta[,1], xlab = colnames(dta)[1], ylab = colnames(dta)[2], log="xy", type="o")
  pred <- predict(lm(log10(dta[,2]) ~ log10(dta[,1])))
  lines(log10(dta[,1]), pred, lty = 1, col = "red")

  return(list(df, D = unname(dval), var = sd(f)))

}
