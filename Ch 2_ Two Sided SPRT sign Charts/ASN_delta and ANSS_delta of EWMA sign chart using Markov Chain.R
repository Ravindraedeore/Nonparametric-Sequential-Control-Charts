############# EWMA Markov Chain for Steady-state ##########
#install.packages("expm")
library(expm)
rm(list=ls())
lambda <- 0.13
k <- 2.867
n <- 10
UCL <- n/2 + k*sqrt((lambda/(2-lambda))*(1/4*n))
CL <- n/2
LCL <- n/2 - k*sqrt((lambda/(2-lambda))*(1/4*n))
LCL;CL;UCL 
m <- 1001
w <- (UCL-LCL)/m
SSARL <- {}
shift <- c(0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.05)
for(l in 1:length(shift))
{
Pz <- matrix(0,nrow=m+1,ncol=m+1) ########For zero-state ARL
Ps <- matrix(0,nrow=m+1,ncol=m+1) ########For Steady-state ARL
O <- {}
for(i in 1:m)
{
  #if(i==1) {O[i]=n/2} else {O[i]=LCL+((2*i-1)/2)*w}
  O[i] <- LCL+((2*i-1)/2)*w
 for(j in 1:m)
 {
  pz1 <- pbinom((LCL+j*w-(1-lambda)*O[i])/lambda,n,0.5)
  pz2 <- pbinom((LCL+(j-1)*w-(1-lambda)*O[i])/lambda,n,0.5)
  Pz[i,j] <- pz1-pz2
  ps1 <- pbinom((LCL+j*w-(1-lambda)*O[i])/lambda,n,shift[l])
  ps2 <- pbinom((LCL+(j-1)*w-(1-lambda)*O[i])/lambda,n,shift[l])
  Ps[i,j] <- ps1-ps2
 }
 pz11 <- pbinom((LCL-(1-lambda)*O[i])/lambda,n,0.5)
 pz22 <- 1-pbinom((UCL-(1-lambda)*O[i])/lambda,n,0.5)
 Pz[i,m+1] <- pz11+pz22
 Pz[m+1,m+1] <- 1
 ps11 <- pbinom((LCL-(1-lambda)*O[i])/lambda,n,shift[l])
 ps22 <- 1-pbinom((UCL-(1-lambda)*O[i])/lambda,n,shift[l])
 Ps[i,m+1] <- ps11+ps22
 Ps[m+1,m+1] <- 1
 }
Q1 <- Ps[1:m,1:m]
T1 <- 1-Pz[,m+1]
T2 <- cbind(Pz[,1:m],T1)
T3 <- T2[,]/T2[,m+1]
#T3 <- Pz[1:m,1:m]/T1[1:m]
T4 <- T3[1:m,1:m]
b11 <- T4%^%100;
b1 <- b11[m,]
I <- diag(m)
Q <- solve(I-Q1)
e <- matrix(rep(1,m),nrow=m,ncol=1)
#print(Q%*%e)
SSARL[l] <- b1%*%Q%*%e
}
cbind(shift,SSARL)
SSANOS <- SSARL*10
a <- cbind(shift,SSARL,SSANOS)
a