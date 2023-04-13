# #
# # test <- Rvcg::vcgSphere(subdivision = 1)
# #
# # l = 0.1
# #
# # test$vb[1:3,] <- l*test$vb[1:3,]/(range(test$vb[1,])[2] - range(test$vb[1,])[1])
# #
# # test$vb[1:3,] <- test$vb[1:3,]
# # test$vb[1:3,] <- test$vb[1:3,] + 3
# #
# # test2 <- Rvcg::vcgSphere(2)
# # test2$vb[1:3,] <- l*test2$vb[1:3,]/(range(test2$vb[1,])[2] - range(test2$vb[1,])[1])
# # test2$vb[1:3,] <- test2$vb[1:3,] + 2.7
# # test2$vb[1:3,] <- test2$vb[1:3,]
# #
# # rgl::plot3d(test)
# # rgl::plot3d(test2)
# # Rvcg::vcgMeshres(test)[[1]]
# # Rvcg::vcgMeshres(test2)[[1]]
# # range(test$vb[1,])[2] - range(test$vb[1,])[1]
# #
# # Rvcg::vcgMeshres(test)[[1]]
# #
# # library(Boov)
# # test3 <- MeshesUnion(list(test, test2))
# # mesh3 <- toRGL(test3)
# # Rvcg::vcgVolume(mesh3)
# # rgl::plot3d(mesh3)
# # library(habtools)
# # m <- Rvcg::vcgQEdecim(mcap,edgeLength = 0.1)
# # rgl::plot3d(m)
# # Rvcg::vcgMeshres(m)
# # d <- t(m$vb)
# # summary(d)
# #
# #
# # fd_bm <- function(data, lvec) {
# #   d <- t(data$vb)
# #   vols <- sapply(lvec, function(l) {
# #     list <- lapply(1:nrow(d), function(i){
# #       s <- Rvcg::vcgSphere(subdivision = 2)
# #       s$vb[1:3,] <- l*s$vb[1:3,]
# #       s$vb[1,] <- s$vb[1,] + d[i,1]
# #       s$vb[2,] <- s$vb[2,] + d[i,2]
# #       s$vb[3,] <- s$vb[3,] + d[i,3]
# #       return(s)
# #     })
# #     un <- Boov::MeshesUnion(list)
# #     mesh <- Boov::toRGL(un)
# #     Rvcg::vcgVolume(mesh)
# #   })
# #   data.frame(l = lvec, volume = vols)
# # }
# #
# # fd_bm(m, 0.05)
# #
# # test <- fd_bm(m, c( 0.05, 0.1, 0.5))
# # plot(log(test))
# #
# # 3 - coef(lm(log(test$volume)~log(test$l)))[2]
# #
# # list <- lapply(1:nrow(dta), function(i){
# #   test <- Rvcg::vcgSphere(subdivision = 1)
# #   test$vb[1:3,] <- 0.2*test$vb[1:3,]/(range(test$vb[1,])[2] - range(test$vb[1,])[1])
# #   test$vb[1,] <- test$vb[1,] + dta[i,1]
# #   test$vb[2,] <- test$vb[2,] + dta[i,2]
# #   test$vb[3,] <- test$vb[3,] + dta[i,3]
# #   return(test)
# # })
# # un <- Boov::MeshesUnion(list)
# # mesh3 <- Boov::toRGL(un)
# # Rvcg::vcgVolume(mesh3)
# #
# # spheres <- rgl::spheres3d(x = dta[,1], y = dta[,2], z = dta[,3], radius = 0.5, fastTransparency = T)
# # ?Rvcg::vcgSphere
# # rgl::plot3d()
# #
#
# ########### smoothening ##########"
# library(raster)
# library(habtools)
# dem <- aggregate(horseshoe, 5, "median")
# y <- disaggregate(dem, 5, method='bilinear')
# plot(horseshoe)
# plot(y)
#
# test <- horseshoe - y
# test2 <- test< (-0.01)
# plot(test2)
# sum(test2@data@values)/length(test@data@values)
# area <- surfaceArea(as(horseshoe, 'SpatialGridDataFrame'), byCell = TRUE)
# hsa <- horseshoe
# hsa@data@values <- area$horseshoe
#
#
# plot(test2)
# plot(hsa * test2)
# test4 <- (hsa * test2)
# test5 <-(horseshoe - a < (-0.01))*hsa
# plot(test5)
# plot(test4)
#
# sum(test4@data@values)/surfaceArea(as(horseshoe, 'SpatialGridDataFrame'))
# sum(test5@data@values, na.rm = T)/surfaceArea(as(horseshoe, 'SpatialGridDataFrame'))
#
# a <- focal(horseshoe, w = matrix(1/25, nc = 5, nr = 5), )
# plot(horseshoe - a < (-0.01))
# plot(test2)
#
# y <- focal(horseshoe, w=matrix(1, 3, 3), max)
# z <- focal(horseshoe, w=matrix(1, 3, 3), min)
# edge <- (y-z)
# plot(edge)
# edge <-  (edge>0.1)
#
# plot(edge)
# sum(edge@data@values, na.rm = T)*0.01*0.01/2
#
# y <- focal(horseshoe, w=matrix(1, 11, 11), median)
# plot(y)
# plot((horseshoe-y)< -0.025)
# plot((horseshoe-y))
# plot((horseshoe-y)> 0.1)
# sum(values(horseshoe-y)< -0.1, na.rm = T)/640000
#
# sobel <- function(r) {
#   fy <- matrix(c(1,0,-1,2,0,-2,1,0,-1), nrow=3)
#   fx <- matrix(c(-1,-2,-1,0,0,0,1,2,1) , nrow=3)
#   rx <- focal(r, fx)
#   ry <- focal(r, fy)
#   sqrt(rx^2 + ry^2)
# }
#
# s <- sobel(horseshoe)
# plot(s)
# plot(s*0.25)
# summary(log(s@data@values))
# t <- terra::terrain(horseshoe, unit = "degrees")
# plot(t)
# summary(s@data@values)
#
# plot(s, edge)
#
# data <- data.frame(
#   zdif = edge@data@values,
#   sobel = s@data@values
# )
#
# library(tidyverse)
#
# data <- data %>%
#   mutate(zdif2 = round(zdif, 1)) %>%
#   dplyr::group_by(zdif2) %>%
#   dplyr::summarize(s = max(sobel), z = mean(zdif))
# plot(data$z, data$s)
#
# lm(data$z~ data$s)
# 0.217+0.1*4.016
#
# plot((s>0.6186) * edge)
#
# sum(values((s>0.6186) * edge) * 0.01*0.01, na.rm = T)
#
#
# m <- matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
# m
#
#
# fy <- matrix(c(1,0,-1,2,0,-2,1,0,-1), nrow=3)
# fx <- matrix(c(-1,-2,-1,0,0,0,1,2,1) , nrow=3)
# rx <- sum(m*fx)
# ry <- sum(m*fy)
# sqrt(rx^2 + ry^2)
#
#
#
#
# sdif <- function(i) {
#   m <- matrix(rnorm(9, 0, 1), nrow = 3, ncol = 3)
#   fy <- matrix(c(1,0,-1,2,0,-2,1,0,-1), nrow=3)
#   fx <- matrix(c(-1,-2,-1,0,0,0,1,2,1) , nrow=3)
#   rx <- sum(m* fx)
#   ry <- sum(m* fy)
#   return(data.frame(rx = rx, ry = ry, s = sqrt(rx^2 + ry^2),
#              d = max(m) - min(m)))
# }
#
# test <- lapply(seq_len(10000), sdif) %>% bind_rows()
#
# lm(d ~ 0 +  s, data = test) %>% summary()
#
# plot(abs(test$ry), y = (test$d))
#
#
#
# #####"variation of sobel
# detect_drop <- function(data, d = 0.1) {
#   fy <- matrix(c(0,0,0,1,0,-1,0,0,0), nrow=3)
#   fx <- matrix(c(0,-1,0,0,0,0,0,1,0) , nrow=3)
#   d1 <- matrix(c(1,0,0,0,0,0,0,0,-1), nrow=3)
#   d2 <- matrix(c(0,0,1,0,0,0,-1,0,0) , nrow=3)
#   rx <- abs(focal(r, fx))
#   ry <- abs(focal(r, fy))
#   rd1 <- abs(focal(r, d1))
#   rd2 <- abs(focal(r, d2))
#   out <- max(rx, ry, rd1, rd2)
#   out * (out > d)
# }
#
#
# plot(detect_drop(horseshoe, 0.25))
# test <- dif(horseshoe)
# test2 <- sobel(horseshoe)
# plot(test)
# plot(test2)
# plot(test*(test>0.1))
# plot(horseshoe)
#
#
# sum(values(test), na.rm = T)
#
#  dem_list <- split_dem(horseshoe, 1)
#  length(dem_list)
#  list <- lapply(dem_list, rdh, lvec = c(0.1, 0.2, 0.4, 0.8)) # apply height range calculation on each tile
# rdh <- bind_rows(list)
#
#
#   m <- dif(horseshoe)
#   m <- m*(m > 0.1)
# plot(m)
# dem2 <- split_dem(m, 1)
# sapply(dem2, function(m){mean(values(m), na.rm = T)})
#
# rdh$o <- sapply(dem2, function(m){sum(values(m)>0, na.rm = T)})
#
# plot((rdh$R), (rdh$o))
# plot(rdh$D, rdh$o)
# plot(rdh$H, log(rdh$o))
#
#
