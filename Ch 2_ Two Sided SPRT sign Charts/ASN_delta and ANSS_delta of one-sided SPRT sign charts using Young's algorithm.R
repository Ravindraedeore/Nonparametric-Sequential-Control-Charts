rm(list=ls())
p0 <- 0.5      ##In-Control value
delta1 <- 0.3   ##Design Shift
p1p <- p0+delta1     ##Upper shift
p1m <- p0-delta1    ##Lower Shift

#####Compute Gamma
r1p <- -log(2-2*p1p)
r2p <- log(p1p/(1-p1p))
r1m <- -log(2-2*p1m)
r2m <- log(p1m/(1-p1m))
gammap <- r1p/r2p
gammam <- r1m/r2m

####Specify In-control ANSS and ASN
ANSS <- 500    ##In-control ANSS 
ANSSp <- 2*ANSS    ##ANSS for upper side
ANSSm <- 2*ANSS    ##ANSS for lower side
ASN <- 10     ##In-control ASN
ASNp <- (ASN+1)/2  ##ASN for upper side
ASNm <- (ASN+1)/2  ##ASN for lower side
alpha <- 1/ANSSp
alpha
#####Find Beta
###install.packages("nleqslv")
library(nleqslv)
fun <- function(betap) ASNp-((alpha*log((1-betap)/alpha)+(1-alpha)*log(betap/(1-alpha)))/(r2p*p0-r1p))
betap <- nleqslv(0.00001,fun,method=c("Broyden", "Newton"))$x
betap

fun <- function(betam) ASNm-((alpha*log((1-betam)/alpha)+(1-alpha)*log(betam/(1-alpha)))/(r2m*p0-r1m))
betam <- nleqslv(0.00001,fun,method=c("Broyden", "Newton"))$x
betam

######Compute bounds
g_U <- log(betap/(1-alpha))/r2p ##Walds Approximate lower bound:upper side
h_U <- log((1-betap)/alpha)/r2p ##Walds Approximate upper bound:upper side
g_L <- log(betam/(1-alpha))/r2m ##Walds Approximate upper bound:lower side 
h_L <- log((1-betam)/alpha)/r2m ##Walds Approximate lower bound:lower side
g_U;h_U;g_L;h_L

############For upper side: one sided chart
g_U <- -1.323
h_U <- 4.797
P <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
ASNu <- {}
ANSSu <- {}
ANOSu <- {}
for(l in 1:length(P))
{
  p <- P[l]
  # the value at which performance measures are to be computed.
  SDp <- {}
  SUp <- {}
  y1p <- {}#Range of Tn-1
  x1p <- {}#Range of Tn
  PTp <- matrix(0,nrow=10000, ncol=10000, byrow=T) # (i,j)th element of PT is P(T(i-1)=j-1
  PTp[1,1] <- 1
  PRp <- {} # prob of reject
  PAp <- {}	# prob of accept
  PTRp <- {} # prob of termination of SPRT
  n <- 1
  prejectp <- 0
  pacceptp <- 0
  pterminatp <- 0
  ASNp <- 0
  while(pterminatp<0.99999)
  {
    if ((g_U+(n*gammap))<0){SDp[n]=as.integer(g_U+(n*gammap))} else {SDp[n]=as.integer(g_U+(n*gammap)+1)}
    SUp[n]=as.integer(h_U+(n*gammap))
    xp=seq(0, n)	#Range of Tn
    x1p=xp[xp>=SDp[n] & xp<=SUp[n]]        
    yp=seq(0,(n-1)) #Range of Tn-1  
    if (n==1){y1p=0} else     
      y1p=yp[yp>=SDp[n-1] & yp<=SUp[n-1]]	
    
    for( t in 1:length(x1p))
    { 
      f={}
      for( k in 1:length(y1p))
      {
        f=matrix(0,nrow=t,ncol=k,byrow=T)
        if(x1p[t]-y1p[k]==0){f[t,k]=(1-p)} # f[t,k] is f(t-k), f is pmf of x (an observation)
        if(x1p[t]-y1p[k]==1){f[t,k]=p}
        if(x1p[t]-y1p[k]>1)	{f[t,k]=0}
        if(x1p[t]-y1p[k]<0) {f[t,k]=0}
        if(n+1>10000 || x1p[t]+1>10000) {break}
        PTp[n+1,x1p[t]+1]=PTp[n+1,x1p[t]+1]+f[t,k]*PTp[n,y1p[k]+1]
      }
    }
    PRp[n]=p*PTp[n,SUp[n]+1]
    if (n==1){PAp[n]=0}
    if(n>1 && SDp[n]==SDp[n-1]){PAp[n]=0} 
    if (n>1 && SDp[n]>SDp[n-1] &&SDp[n-1]<0){PAp[n]=0}
    if (n>1 && SDp[n]>SDp[n-1] &&SDp[n-1]>=0){PAp[n]=(1-p)*PTp[n,SDp[n-1]+1]}
    PTRp[n]=PRp[n]+PAp[n]
    prejectp=prejectp+PRp[n]
    pacceptp=pacceptp+PAp[n]
    pterminatp=pacceptp+prejectp
    ASNp=ASNp+n*(PTRp[n])
    n=n+1
    #if(n>10000) {break}
  }
  ANSSp <- 1/prejectp
  ANSSu[l] <- ANSSp
  ASNu[l] <- ASNp
  ANOSu[l] <- ASNp*ANSSp
}

###########For lower side: one sided chart Final programme
g_L <- -g_U #1.323
h_L <- -h_U #-4.797
P <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
ASNl <- {}
ANSSl <- {}
ANOSl <- {}
for(l in 1:length(P))
{
  p <- P[l]  
  # the value at which performance measures are to be computed.
  SDm <- {}
  SUm <- {}
  y1m <- {}#Range of Tn-1
  x1m <- {}#Range of Tn
  PTm <- matrix(0,nrow=10000, ncol=10000, byrow=T) # (i,j)th element of PT is P(T(i-1)=j-1
  PTm[1,1] <- 1
  PRm <- {}    # prob of reject
  PAm <- {}	# prob of accept
  PTRm <- {}   # prob of termination of SPRT
  n <- 1
  prejectm <- 0
  pacceptm <- 0
  pterminatm <- 0
  ASNm <- 0
  while(pterminatm<0.99999)
  {
    if ((h_L+(n*gammam))<0){SDm[n]=as.integer(h_L+(n*gammam))} else {SDm[n]=as.integer(h_L+(n*gammam)+1)}
    SUm[n]=as.integer(g_L+(n*gammam))
    xm=seq(0,n)	#Range of Tn seq(1:1)
    x1m=xm[xm>=SDm[n] & xm<=SUm[n]]        
    ym=seq(0,(n-1)) #Range of Tn-1  
    if (n==1){y1m=0} else     
    {y1m=ym[ym>=SDm[n-1] & ym<=SUm[n-1]]}	
    
    for( t in 1:length(x1m))
    { 
      f={}
      for( k in 1:length(y1m))
      {
        f=matrix(0,nrow=t,ncol=k,byrow=T)
        if(x1m[t]-y1m[k]==0) {f[t,k]=(1-p)}## f[t,k] is f(t-k), f is pmf of x (an observation)
        if(x1m[t]-y1m[k]==1){f[t,k]=p}
        if(x1m[t]-y1m[k]>1)	{f[t,k]=0}
        if(x1m[t]-y1m[k]<0) {f[t,k]=0}
        PTm[n+1,x1m[t]+1]=PTm[n+1,x1m[t]+1]+f[t,k]*PTm[n,y1m[k]+1]
      }
    }
    if (SDm[n]<=0) {PRm[n]=0}
    if (SDm[n]>0) {PRm[n]= (1-p)*PTm[n,SDm[n]]}
    PAm[n]=p*PTm[n,SUm[n]+1]
    PTRm[n] = PRm[n] + PAm[n]
    prejectm = prejectm + PRm[n]
    pacceptm = pacceptm + PAm[n]
    pterminatm = pacceptm + prejectm
    ASNm = ASNm + n*(PTRm[n])
    n=n+1
  }
  ANSSm <- 1/prejectm
  ANSSl[l] <- ANSSm
  ASNl[l] <- ASNm
  ANOSl[l] <- ASNm*ANSSm
}
aASN <- ASNu+ASNl-1
aANSS <- 1/(1/ANSSu+1/ANSSl)
aANOS <- aASN*aANSS
cbind(P,aANSS,aASN,aANOS)