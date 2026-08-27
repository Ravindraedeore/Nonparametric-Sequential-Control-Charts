################For upper side: For All p : Steady State
#install.packages("expm")
library(expm)
rm(list=ls())
n <- 10
k <- n*0.15/2
h <- 8.25
N <- 1000
d <- h/(2*(N-1))
ARLu <- {}
m <- {}
for(i in 1:N)
{
 if(i==1) {m[i]=0} else{m[i]=(2*i-3)*h/(2*(N-1))}
}
Pu <- c(0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95, 0.45,0.40,0.35,0.30,0.25,0.20,0.15,0.10,0.05)
for(l in 1:length(Pu))
{
 PS <- matrix(0,nrow=N,ncol=N)
 T <- matrix(0,nrow=N,ncol=N)
 for(i in 1:(N))
 {
  for(j in 1:(N))
  {
   if(j==1) {PS[i,j]=pbinom(-m[i]+n*0.5+k,n,0.5)}
   if(j>1) {if(is.integer(m[j]-m[i]+d+n*0.5+k)) {p1S=pbinom(m[j]-m[i]+d+n*0.5+k-1,n,0.5)} else {p1S=pbinom(m[j]-m[i]+d+n*0.5+k,n,0.5)}
      if(is.integer(m[j]-m[i]-d+n*0.5+k)){p2S=pbinom(m[j]-m[i]-d+n*0.5+k-1,n,0.5)} else {p2S=pbinom(m[j]-m[i]-d+n*0.5+k,n,0.5)}
      PS[i,j]=p1S-p2S}
   if(j==1) {T[i,j]=pbinom(-m[i]+n*0.5+k,n,Pu[l])}
   if(j>1) {if(is.integer(m[j]-m[i]+d+n*0.5+k)) {p1=pbinom(m[j]-m[i]+d+n*0.5+k-1,n,Pu[l])} else {p1=pbinom(m[j]-m[i]+d+n*0.5+k,n,Pu[l])}
      if(is.integer(m[j]-m[i]-d+n*0.5+k)){p2=pbinom(m[j]-m[i]-d+n*0.5+k-1,n,Pu[l])} else {p2=pbinom(m[j]-m[i]-d+n*0.5+k,n,Pu[l])}
      T[i,j]=p1-p2}
  }
 PS[i,] <- PS[i,]/sum(PS[i,])
 }
 b1 <- PS%^%100
 b <- b1[1,]
 I <- diag(N)
 Q <- solve(I-T)
 e <- matrix(rep(1,N),nrow=N,ncol=1)
 ARLu[l] <- b%*%Q%*%e
}
#cbind(Pu,ARLu)
for(i in 1:length(Pu))
{
 if(i>length(ARLu)) {ARLu=c(ARLu,Inf)}
}

#####For lower side: For All P 
n <- 10
k <- 10*0.15/2
h <- 8.25
N <- 1000
d <- h/(2*(N-1))
ARLl <- {}
m <- {}
for(i in 1:N)
{
 if(i==1) {m[i]=0} else{m[i]=-(2*i-3)*h/(2*(N-1))}
}
library(expm)
Pl <- c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.55, 0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
for(l in 1:length(Pl))
{
 PS <- matrix(0,nrow=N,ncol=N)
 T <- matrix(0,nrow=N,ncol=N)
 for(i in 1:(N))
 {
  for(j in 1:(N))
  {
   if(j==1) {if(is.integer(-m[i]+n*0.5-k)) {PS[i,j]=1-pbinom(-m[i]+n*0.5-k-1,n,0.5)} else {PS[i,j]=1-pbinom(-m[i]+n*0.5-k,n,0.5)}}
   if(j>1) {PS[i,j]=pbinom(m[j]-m[i]+d+n*0.5-k,n,0.5) - pbinom(m[j]-m[i]-d+n*0.5-k,n,0.5)}
   if(j==1) {if(is.integer(-m[i]+n*0.5-k)) {T[i,j]=1-pbinom(-m[i]+n*0.5-k-1,n,Pl[l])} else {T[i,j]=1-pbinom(-m[i]+n*0.5-k,n,Pl[l])}}
   if(j>1) {T[i,j]=pbinom(m[j]-m[i]+d+n*0.5-k,n,Pl[l]) - pbinom(m[j]-m[i]-d+n*0.5-k,n,Pl[l])}
   #if(j>1) {if(is.integer(m[j]-m[i]+d+n*0.5-k)) {p1=pbinom(m[j]-m[i]+d+n*0.5-k-1,n,Pl[l])} else {p1=pbinom(m[j]-m[i]+d+n*0.5-k,n,Pl[l])}
          # if(is.integer(m[j]-m[i]-d+n*0.5-k)){p2=pbinom(m[j]-m[i]-d+n*0.5-k-1,n,Pl[l])} else {p2=pbinom(m[j]-m[i]-d+n*0.5-k,n,Pl[l])}
          #T[i,j]=p1-p2}
  }
 PS[i,] <- PS[i,]/sum(PS[i,])
 }
 b1 <- PS%^%100
 b <- b1[1,]
 I <- diag(N)
 Q <- solve(I-T)
 e <- matrix(rep(1,N),nrow=N,ncol=1)
 ARLl[l] <- b%*%Q%*%e
}

for(i in 1:length(Pl))
{
 if(i>length(ARLl)) {ARLl=c(ARLl,Inf)}
}

###For two-sided
P=c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.55, 0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
ARL={}
for(i in 1:length(P))
{
 for(j in 1:length(P))
 {
  if(Pl[i]==Pu[j]) {ARL[i]=1/(1/ARLl[i]+1/ARLu[j])}
 }
}
cbind(P,ARL)