#' Investigates varying fractal dimensions across scale
#'
#' @param data Digital elevation model of class RasterLayer
#' @param lvec scales to use for calculation
#' @param method Possible methods are:"hvar", "area", and "sd".
#' @param ... Additional arguments of see [fd()]
#' @return A list with fractal dimension across scales, mean fractal dimension, and sd of fractal dimensions across scales.
#' @export
#'
#' @importFrom graphics axis points
#'
#' @examples
#' fd_diagnose(horseshoe, lvec = c(0.05, 0.1, 0.2, 0.4), method = "area")
#'
fd_diagnose <- function(data, lvec, method, ...) {

  lvec <- sort(lvec)
  d <- fd(data = data, lvec = lvec, method = method, keep_data = TRUE, ...)

  dta <- d[["data"]]
  dval <- d[["fd"]]

  f <- diff(log10(dta[,2])) / diff(log10(dta[,1]))
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

  plot(dta[,2] ~ dta[,1], xlab = colnames(dta)[1], ylab = colnames(dta)[2], log="xy", type="l", lty=2, col="grey", axes=FALSE, main=method)
  axis(1)
  axis(2, las=2)
  points(dta[,2] ~ dta[,1])
  text(midv(dta[,1]), midv(dta[,2]), round(f, 2))
  pred <- predict(lm(log10(dta[,2]) ~ log10(dta[,1])))
  lines(dta[,1], 10^pred, lty = 1, col = "red")
  if (method == "hvar") {
    legend("bottomright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
  } else {
    legend("topright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
  }
  return(list(df, D = unname(dval), var = sd(f)))

}

midv <- function(v) { v[-length(v)] + diff(v)/2 }
