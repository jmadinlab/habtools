fd_mesh <- function(m, n = 10, fold = 100, cores = 4, plot = T){
  r0 <- Rvcg::vcgMeshres(m)$res
  r <- exp(seq(log(r0), log(fold*r0),  (log(fold*r0) - log(r0))/(n-1))[-1])

  r <- 10^(seq(log10(0.02), log10(0.08),  (log10(0.08) - log10(0.02))/5))

  a1 <-  data.frame(res = r0,
                    area = Rvcg::vcgArea(m))

  a <- parallel::mclapply(r, function(l){
    test2 <- Rvcg::vcgQEdecim(m, edgeLength = l)
    res <- Rvcg::vcgMeshres(test2)$res
    area <- Rvcg::vcgArea(test2)
    data.frame(res = res, area = area)
  }, mc.cores = cores) %>% plyr::ldply()

  a <- rbind(a1, a)
  if (plot){
    plot(log10(a$res), log10(a$area))
  }

  fd <- 2  - lm(log10(a$area) ~ log10(a$res))$coefficients[2]
  fd
}


