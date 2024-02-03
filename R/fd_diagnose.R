#' Investigate varying fractal dimensions across scale
#'
#' @param data Output of [fd()] with option keep_data = TRUE.
#' @param keep_data Logical. Keep diagnostics data?
#' @return A list with fractal dimension across scales, mean fractal dimension, and sd of fractal dimensions across scales.
#' @export
#'
#' @importFrom graphics axis points
#'
#' @examples
#' fd_data <- fd(horseshoe, lvec = c(0.05, 0.1, 0.2, 0.4), method = "area", keep_data = TRUE)
#' fd_diagnose(fd_data)
#' fd_diagnose(fd_data, keep_data = FALSE)
#'
fd_diagnose <- function(data, keep_data = TRUE) {

  dta <- data[["data"]]
  dta <- dta[order(dta$l),]
  dval <- data[["fd"]]
  method <- data[["method"]]

  f <- diff(log10(dta[,2])) / diff(log10(dta[,1]))
  if (method == "area") {
    f <- 2 - f
  } else if (method == "cubes") {
    f <- -f
  } else {
    f <- 3 - f
  }
  plot(dta[,2] ~ dta[,1], xlab = colnames(dta)[1], ylab = colnames(dta)[2], log="xy", type="l",
       lty=2, col="grey", axes = FALSE, main = paste0('Method: "', method, '"'))
  axis(1)
  axis(2, las=2)
  points(dta[,2] ~ dta[,1])
  text(midv(dta[,1]), midv(dta[,2]), round(f, 2), cex=0.8)
  pred <- predict(lm(log10(dta[,2]) ~ log10(dta[,1])))
  lines(dta[,1], 10^pred, lty = 1, col = "red")
  if (method %in% c("hvar", "sd")) {
    legend("bottomright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
  } else {
    legend("topright", legend=c(paste0("D = ", round(dval, 2)), paste0("var = ", round(sd(f), 2))), bty="n")
  }
  if (keep_data) {
    return(list(D = unname(dval), data = dta, D_vec = f, var = sd(f), method = method))
  }
}


midv <- function(v) { v[-length(v)] + diff(v)/2 }
