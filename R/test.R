#
# test <- Rvcg::vcgSphere(subdivision = 1)
#
# l = 0.1
#
# test$vb[1:3,] <- l*test$vb[1:3,]/(range(test$vb[1,])[2] - range(test$vb[1,])[1])
#
# test$vb[1:3,] <- test$vb[1:3,]
# test$vb[1:3,] <- test$vb[1:3,] + 3
#
# test2 <- Rvcg::vcgSphere(2)
# test2$vb[1:3,] <- l*test2$vb[1:3,]/(range(test2$vb[1,])[2] - range(test2$vb[1,])[1])
# test2$vb[1:3,] <- test2$vb[1:3,] + 2.7
# test2$vb[1:3,] <- test2$vb[1:3,]
#
# rgl::plot3d(test)
# rgl::plot3d(test2)
# Rvcg::vcgMeshres(test)[[1]]
# Rvcg::vcgMeshres(test2)[[1]]
# range(test$vb[1,])[2] - range(test$vb[1,])[1]
#
# Rvcg::vcgMeshres(test)[[1]]
#
# library(Boov)
# test3 <- MeshesUnion(list(test, test2))
# mesh3 <- toRGL(test3)
# Rvcg::vcgVolume(mesh3)
# rgl::plot3d(mesh3)
# library(habtools)
# m <- Rvcg::vcgQEdecim(mcap,edgeLength = 0.1)
# rgl::plot3d(m)
# Rvcg::vcgMeshres(m)
# d <- t(m$vb)
# summary(d)
#
#
# fd_bm <- function(data, lvec) {
#   d <- t(data$vb)
#   vols <- sapply(lvec, function(l) {
#     list <- lapply(1:nrow(d), function(i){
#       s <- Rvcg::vcgSphere(subdivision = 2)
#       s$vb[1:3,] <- l*s$vb[1:3,]
#       s$vb[1,] <- s$vb[1,] + d[i,1]
#       s$vb[2,] <- s$vb[2,] + d[i,2]
#       s$vb[3,] <- s$vb[3,] + d[i,3]
#       return(s)
#     })
#     un <- Boov::MeshesUnion(list)
#     mesh <- Boov::toRGL(un)
#     Rvcg::vcgVolume(mesh)
#   })
#   data.frame(l = lvec, volume = vols)
# }
#
# fd_bm(m, 0.05)
#
# test <- fd_bm(m, c( 0.05, 0.1, 0.5))
# plot(log(test))
#
# 3 - coef(lm(log(test$volume)~log(test$l)))[2]
#
# list <- lapply(1:nrow(dta), function(i){
#   test <- Rvcg::vcgSphere(subdivision = 1)
#   test$vb[1:3,] <- 0.2*test$vb[1:3,]/(range(test$vb[1,])[2] - range(test$vb[1,])[1])
#   test$vb[1,] <- test$vb[1,] + dta[i,1]
#   test$vb[2,] <- test$vb[2,] + dta[i,2]
#   test$vb[3,] <- test$vb[3,] + dta[i,3]
#   return(test)
# })
# un <- Boov::MeshesUnion(list)
# mesh3 <- Boov::toRGL(un)
# Rvcg::vcgVolume(mesh3)
#
# spheres <- rgl::spheres3d(x = dta[,1], y = dta[,2], z = dta[,3], radius = 0.5, fastTransparency = T)
# ?Rvcg::vcgSphere
# rgl::plot3d()
#
