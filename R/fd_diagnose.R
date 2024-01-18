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
  d <- fd(data = data, lvec = lvec, method = method, keep_data = T, ...)

  dta <- d[["data"]]
  dval <- d[["fd"]]

  f <- unname(sapply(1:(length(dta[,1]) - 1), function(i){
    s <- coef(lm(log10(dta[,2][c(i,i+1)]) ~ log10(dta[,1][c(i,i+1)])))[2]
    if (method == "area") {
      2 - s
    } else {
      3 - s
    }
  }))

  df <- data.frame(l = dta[,1][1:length(dta[,1])-1], fd = f)

  plot(log10(dta[,2]) ~ log10(dta[,1]), xlab = colnames(dta)[1], ylab = colnames(dta)[2])
  lines(log10(dta[,2]) ~ log10(dta[,1]))
  pred <- predict(lm(log10(dta[,2]) ~ log10(dta[,1])))
  lines(log10(dta[,1]), pred, lty = 1, col = "red")

  return(list(df, D = unname(dval), var = sd(f)))

}
