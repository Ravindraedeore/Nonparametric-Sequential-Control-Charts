n <- 10
a <- 10
P <- c(0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
ANSSu <- {}
ANSSl <- {}
for(i in 1:length(P))
{
 pu <- 1-pbinom(a-1,n,P[i])
 pl <- pbinom(n-a,n,P[i])
 ANSSu[i] <- 1/pu
 ANSSl[i] <- 1/pl
}
cbind(P,ANSSu,ANSSl)
ANSS=1/(1/ANSSu+1/ANSSl)
ANOS=ANSS*n
cbind(P,ANSS,ANOS)