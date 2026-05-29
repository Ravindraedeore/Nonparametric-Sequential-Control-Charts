#library(VGAM)
rm(list=ls())

start_time <- Sys.time()

mu <- 0
sigma0 <- 1
delta <- c(1.25,1.50,1.75,2.00,2.50,3.00,3.50,4.00,5.00)

sigma1 <- sigma0*delta

m <- 100              ###Reference sample size
a=0.201
b=2.843
simu_runs <- 10000
truncation=500
ARL <- {}
ASN <- {}
for(z1 in 1:length(delta))
{
  RL <- {}
  SN1 <- {}
  zj <- (a+b)/2   ### Initial value of zj
  for(i in 1:simu_runs)
  {
    SN2 <- {}
    t <- 0  #Counter for RL
    zj <- (a+b)/2
    rs <- rnorm(m,mu,sigma0)
    rs1 <- rs[rs<mu]
    rs2 <- rs[rs>mu]
    while(zj<b)
    {
      j <- 0
      x <- {}#rnorm(1,mu,sigma1)#{}
      T <- 0
      zj <- (a+b)/2    ### To reset the value of zj
      while(zj>a && zj<b && j<truncation)
      {
        x1 <- x[x<mu]
        x2 <- x[x>mu]
        x <- c(x,rnorm(1,mu,sigma1[z1]))
        j <- length(x)
        
        if(x[j]<mu && length(rs1)>0)
        {
          for (l in 1:length(rs1))
          {
            if(x[j]<rs1[l]) {T <- T+1} #{T <- T}
          }
        }  
        if(x[j]>mu && length(rs2)>0)
        {
          for(ll in 1:length(rs2))
          {
            if(x[j]>rs2[ll]) {T <- T+1}
          }
         }
        mn <- (m*j)/4
        sd <- sqrt((m*j*(m+j+7))/48)
        zj <- (T-mn)/sd
      }
      SN2 <- c(SN2,j)
      t <- t+1
    }
    RL[i] <- t
    SN1 <- c(SN1,SN2)
    #print(cbind(z1,i))
  }
  ARL[z1] <- mean(RL)
  ASN[z1] <- mean(SN1)
}
ANOS <- ARL*ASN
cbind(m,a,b,ASN,ARL,ANOS,simu_runs,truncation)
end_time <- Sys.time()
end_time-start_time
