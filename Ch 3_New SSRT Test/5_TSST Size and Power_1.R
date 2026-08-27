######################Power and ASN TSST
rm(list=ls())
mu0 <- 0
mu <- c(0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00)
p0 <- 0.5      ##In-Control value
p1p <- 0.8     ##Upper shift
p1m <- 0.2    ##Lower Shift
alpha <- 0.01/2 #### this is for 1-sided, means for 2-sided alpha2=2*alpha
beta <- 0.6011568 ###Beta from above equation given by Wald
#####Compute Gamma
r1p <- -log(2-2*p1p)
r2p <- log(p1p/(1-p1p))
r1m <- -log(2-2*p1m)
r2m <- log(p1m/(1-p1m))
gammap <- r1p/r2p
gammam <- r1m/r2m
gammap;gammam
g <- log(beta/(1-alpha))/r2p
h <- log((1-beta)/alpha)/r2p
g;h
g <- -1.847   #-1.047   #-1.920
h <-  3.502   #3.744    #2.441
pow <- {}
ASN <- {}
simulation_runs <- 100000
for(l in 1:length(mu))
{
 A <- 0
 R <- 0
 ASN1 <- {}
 p <- 1-pnorm(mu0,mu[l],1)
 #p <- 1-plaplace(mu0,mu[l],1/sqrt(2))
 #p <- 1-pcauchy(mu0,mu[l],0.2605)
 #p <- 1-punif(mu0,-sqrt(3)+mu[l],sqrt(3)+mu[l])
 #p <- 1-plogis(mu0,mu[l], sqrt(3)/pi)
 for(i in 1:simulation_runs)
 {
  SN <- 0
  Tu <- 0
  Tl <- 0
  while( Tu<h  && Tl<h && (Tu>g || Tl>g))
  {
   u <- rbinom(1,1,p)
   if(Tu<g) {Tu=Tu}  else {Tu = Tu + u - gammap}
   if(Tl<g) {Tl=Tl} else  {Tl = Tl - u + gammam}
   SN <- SN+1
  }
  if(Tu>=h  || Tl>=h) {R=R+1} else {A=A+1}
  ASN1[i] <- SN
  #print(cbind(l,i))
 }
 pow[l] <- R/simulation_runs
 ASN[l] <- mean(ASN1)
}
cbind(mu,pow,ASN)