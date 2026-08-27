###EWMA WSR - Steady State State
rm(list=ls())
start_time=Sys.time()
mu0 <- 0.00
sigma0 <- 1
lambda <- 0.20            ###Chart parameter
k <- 2.915;n <- 10
UCL <- n*(n+1)/4 + k*sqrt((lambda/(2-lambda))*(n*(n+1)*(2*n+1)/24))
CL <- n*(n+1)/4
LCL <- n*(n+1)/4- k*sqrt((lambda/(2-lambda))*(n*(n+1)*(2*n+1)/24))
shift <- c(0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00)
ARL <- {}
for(j in 1:length(shift))
 {
 ARL1 <- {}
 i <- 1
 while( i <= 100000)
 {
  t <- 0
  EWMA <- n*(n+1)/4
  while(EWMA<UCL && EWMA>LCL)
  {
   if(t<100) {x <- rnorm(n,mu0,sigma0)} else {x <- rnorm(n,mu0+shift[j],sigma0)}
   sign0 <- sign(x-mu0)
   abs0 <- abs(x-mu0)
   r0 <- rank(abs0)
   rank0 <- sign0*r0
   SRp <- sum(rank0[rank0>0])
   EWMA <- lambda*SRp + (1-lambda)*EWMA
   t=t+1
  }
  if(t<100) {i=i}
  if(t>=100) {ARL1[i] = t}
  if(t>=100) {i=i+1}
  #print(c(i,t))
 }
 ARL[j] <- mean(ARL1-100)
 print(j)
}
ANOS <- ARL*n
cbind(shift,ARL,ANOS)
end_time <- Sys.time()
end_time-start_time