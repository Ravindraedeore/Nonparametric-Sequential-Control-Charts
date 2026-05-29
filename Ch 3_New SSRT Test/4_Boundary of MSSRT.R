############ For finding the values of |Z|alphaN 
rm(list=ls())
alpha=0.01 ###Size of the test
N=10      ###Truncation number for sample size
Z={}
mu0=0
for(i in 1:100000)
{
 x={}              #####vector for random sample
 Yn={}             #####vector for z transformation
 for(n in 1:N)
 {
  x=c(x,rnorm(1,mu0,1))
  sign0=sign(x-mu0)
  abs0=abs(x-mu0)
  r0=rank(abs0)
  rank0=sign0*r0
  SRp=sum(rank0[rank0>0])  
  
  Yn[n]=(SRp-n*(n+1)/4)/sqrt(n*(n+1)*(2*n+1)/24)
 }
 Z[i]=max(abs(Yn))

}
Zalpha=quantile(Z,1-alpha)
Zalpha