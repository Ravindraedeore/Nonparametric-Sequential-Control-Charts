
#####Simulation for Size and Power of SSRT: Combined ASN and power

rm(list=ls())
start_time=Sys.time()

mu0=0
mu=c(0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00)
a=0.67
b=2.80
N=10000
pow={}
ASN={}
for(k in 1:length(mu))
{
  A=0
  R=0
  SN={}
  for(i in 1:N)
  {
    x={}
    j=0
    zj=(a+b)/2
    while(abs(zj)>a && abs(zj)<b && j<=500)
    {
      x=c(x,rnorm(1,mu[k],1))
      sign0=sign(x-mu0)
      abs0=abs(x-mu0)
      r0=rank(abs0)
      rank0=sign0*r0
      SRp=sum(rank0[rank0>0])
      j=j+1
      zj=(SRp-j*(j+1)/4)/sqrt(j*(j+1)*(2*j+1)/24)
    }
    if(abs(zj)<=a) {A=A+1} else {R=R+1}
    #print(i)
    SN[i]=j
  }
  pow[k]=R/N

  ASN[k]=mean(SN)
  print(k)
}
cbind(a,b,pow,ASN)
#cbind(mu,pow,ASN)
end_time=Sys.time()
end_time-start_time

