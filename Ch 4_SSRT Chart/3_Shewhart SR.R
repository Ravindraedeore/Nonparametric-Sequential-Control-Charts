rm(list=ls())
library(VGAM)
start_time <- Sys.time()
mu0 <- 0
mu1 <- 0.00
n <- 10
UCL <- 55
LCL <- 0
shift <- c(0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.0,2.5,3.0)
simulation_runs <- 100000
ARL <- {}
for(j in 1:length(shift))
{
  ARL1 <- {}
  for(i in 1:simulation_runs)
  {
    t <- 0
    wsr <- (UCL+LCL)/2
    while(wsr>LCL && wsr<UCL)
    { 
      x <- rnorm(n,shift[j],1)
      #x <- rcauchy(n,shift[j],0.2605)
      #x <- rlaplace(n,shift[j],1/sqrt(2))
      #x <- runif(n,-sqrt(3)+shift[j],sqrt(3)+shift[j])
      sign0 <- sign(x-mu0)
      abs0 <- abs(x-mu0)
      r0 <- rank(abs0)
      rank0 <- sign0*r0
      wsr <- sum(rank0[rank0>0])
      t <- t+1  
    }
    ARL1[i] <- t
  }
  ARL[j] <- mean(ARL1)
  print(j)
}
ANOS <- ARL*n
cbind(shift,ARL,ANOS)
end_time <- Sys.time()
end_time-start_time
