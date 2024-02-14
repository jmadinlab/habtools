#
# sim <- function(n=50, L = 100, H = 1, R = 2, f = 3, sigma = 1,
#                 plot = TRUE, noise = TRUE, noise_prop = 0.05) {
#   mat <- matrix(rlnorm(L*L, 0, sigma), L, L)
#   r <- raster(mat)
#   if (plot) {
#     plot(r, legend=FALSE, asp=NA)
#   }
#   w <- matrix(1, nr=f,nc=f)
#   for (i in 1:n) {
#     if (noise) {
#       no <- rnorm(L*L, 1, H/10)
#       no[sample(1:(L*L), size = L*L*(1 - noise_prop))] <- 1
#       r@data@values <- r@data@values * no
#     }
#     r <- focal(r, w=w,  fun = mean, na.rm = TRUE, pad = T)
#     if (plot) {
#       plot(r, legend=FALSE, asp=NA, main=i)
#     }
#     dev.flush()
#     r@data@values[is.na(r@data@values)] <- mean(r@data@values, na.rm = T)
#     r@data@values <- r@data@values * H/hr(r)
#     r@data@values <- r@data@values - min(r@data@values)
#     ru <- rg(r, L0 = 10/L, method = "hvar", parallel = T)
#     print(ru)
#     if (R == round(ru, 1)) {
#       break
#     }
#   }
#   if(!R == round(ru, 1)) {
#     stop("Run for more iterations or change parameters")
#   }
#   return(r)
# }
#
# test <- sim(n = 100, L = 100,  H = 1, R = 3, f = 3, sigma = 0.5)
# projection(test) <- "+proj=tmerc +datum=WGS84"
# # test2 <- sim(n = 100, L = 100,  H = 2, R = 3, f = 3, sigma = 0.5, pause = 0.1)
# # test3 <- sim(n = 100, L = 100,  H = 3, R = 3, f = 11, sigma = 1, pause = 0.1, noise = T, noise_prop = 0.5)
# #
# # plot(test)
# # plot(test2)
# # plot(test3)
# #
# # rdh(test, lvec = c(0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test2, lvec = c(0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test3, lvec = c(0.1, 0.2, 0.5, 1), method = "hvar")
# #
# test <- sim(n = 100, L = 200,  H = 2, R = 2, f = 15, sigma = 2, noise = T, noise_prop = 0.5)
# # test2 <- sim(n = 100, L = 200,  H = 2, R = 3, f = 7, sigma = 0.1)
# # test3 <- sim(n = 100, L = 200,  H = 2, R = 4, f = 5, sigma = 0.5, noise = T, noise_prop = 0.05)
# # test4 <- sim(n = 100, L = 200,  H = 2, R = 5, f = 3, sigma = 0.1, noise = T, noise_prop = 0.05)
# # test4 <- sim(n = 100, L = 200,  H = 2, R = 6, f = 3, sigma = 0.1, noise = T, noise_prop = 0.05)
# # test7 <- sim(n = 100, L = 200,  H = 2, R = 7, f = 3, sigma = 0.1, noise = T, noise_prop = 0.05)
# # test8 <- sim(n = 100, L = 200,  H = 2, R = 8, f = 3, sigma = 0.2, noise = T, noise_prop = 0.05)
# # test10 <- sim(n = 100, L = 200,  H = 2, R = 10, f = 3, sigma = 0.2, noise = T, noise_prop = 0.05)
#  test12 <- sim(n = 100, L = 200,  H = 2, R = 12, f = 3, sigma = 0.1, noise = T, noise_prop = 0.05)
# #
#  plot(test)
# # plot(test2)
# # plot(test3)
# # plot(test4)
# # plot(test8)
# # plot(test12)
# #
#  rdh(test, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test2, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test3, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test4, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test7, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test8, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(test10, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
#  rdh(test12, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
# # rdh(r, lvec = c(0.05, 0.1, 0.2, 0.5, 1), method = "hvar")
#
#
