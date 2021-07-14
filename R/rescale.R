rescale <- function(x, x0, xm, n) {
  temp <- (x - x0)/(xm - x0)*n
  return(temp)
}
