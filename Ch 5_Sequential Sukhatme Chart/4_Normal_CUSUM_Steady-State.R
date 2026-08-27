#library(VGAM)
rm(list=ls())
start_time <- Sys.time()
mu <- 0
sigma0 <- 1
delta <- c(1.25,1.50,1.75,2.00,2.50,3.00,3.50,4.00,5.00)
sigma1 <- sigma0*delta
m <- 100             ###Reference sample size
n <- 10
simulation_runs <- 1000
k1 <- 1.00
h <- 1.62
ARL <- {}
for(j in 1:length(sigma1))
{
 ARL1 <- {}
 i <- 0
 while(i<simulation_runs)
 {
  rs <- rnorm(m,mu,sigma0)
  rs1 <- rs[rs<mu]
  rs2 <- rs[rs>mu]
  t <- 0
  St <- 0
  while(St<h)
  {
   if(t<100) {x <- rnorm(n,mu,sigma0)} else {x <- rnorm(n,mu,sigma1[j])}
   T <- 0
   for(k in 1:n)
   {
    if(x[k]<mu && length(rs1)>0)
     {
      for (l in 1:length(rs1))
      {
       if(x[k]<rs1[l]) {T <- T+1}
      }
     }  
     if(x[k]>mu && length(rs2)>0)
     {
      for(ll in 1:length(rs2))
      {
       if(x[k]>rs2[ll]) {T <- T+1}
      }
     }
   }
   mn <- (m*n)/4
   sd <- sqrt((m*n*(m+n+7))/48)
   zj <- (T-mn)/sd
   St <- max(0,St+zj-k1)   
   t <- t+1
   if(t>=5000) {break}
  }
  if(t<=100) {i=i}
  if(t>100){ARL1[i]=t-100}
  if(t>100){i=i+1}
  #print(i)
 }
 ARL[j] <- mean(ARL1)
 print(j)
}
ANOS <- ARL*n
cbind(delta,ARL,ANOS)
end_time <- Sys.time()
end_time-start_time