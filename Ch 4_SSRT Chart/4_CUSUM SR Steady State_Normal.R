rm(list=ls())
start_time <- Sys.time()                         
n <- 10                                   
mu0 <- 0                
sigma0 <- 1
h <- 41.0
k <- 6        #k= reference value 
shift <- c(0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.0,2.5,3.0)
ARL <- {}
for(j in 1:length(shift))
{  
  ARL1 <- {}
  i <- 0
  while(i<100000)
  {
    t <- 0
    ci_positive <- 0
    ci_minus <- 0
    wsr <- 0
    while(ci_positive<h && ci_minus<h)
    {     
      if(t<100) {x <- rnorm(n,mu0,sigma0)} else{x <- rnorm(n,mu0+shift[j],sigma0)}
      sign0 <- sign(x-mu0)
      abs0 <- abs(x-mu0)
      r0 <- rank(abs0)
      rank0 <- sign0*r0
      wsr <- sum(rank0[rank0>0]) 
      ci_positive <- max(0,wsr-(n*(n+1)/4+k)+ci_positive)
      ci_minus <- max(0,(n*(n+1)/4-k)-wsr+ci_minus)
      t <- t+1
    }
    if(t<100) {i=i}
    if(t>100){ARL1[i]=t-100}
    if(t>100){i=i+1}
  }
  ARL[j] <- mean(ARL1)
  print(j)
}
ANOS <- ARL*n
cbind(shift,ARL,ANOS)
end_time <- Sys.time()
end_time-start_time