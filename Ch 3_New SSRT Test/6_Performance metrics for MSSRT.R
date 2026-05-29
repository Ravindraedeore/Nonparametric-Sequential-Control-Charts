#######Performance metrics for MSSRT ##################
rm(list=ls())
N=20
Zalpha=2.80
start_time=Sys.time()
mu0=0.00
mu=c(0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00)

pow={}
ASN={}
simulation_runs=10000
for(k in 1:length(mu))
{
 A=0
 R=0 
 #ASN1={}
 SN={}
 t=0
 for(i in 1:simulation_runs)
 {
  T=0
  Tcritical=1
  x={}               ###vector for random sample
  n=0
  while(T<Tcritical && n<N)
   {
    x=c(x,rnorm(1,mu[k],1))
    #x=c(x,rlaplace(1,mu[k],1/sqrt(2)))
    #x=c(x,rcauchy(1,mu[k],0.2605))
    #x=c(x,runif(1,-sqrt(3)+mu[k],sqrt(3)+mu[k]))
    #x=c(x,rlogis(1,mu[k],sqrt(3)/pi))
    sign0=sign(x-mu0)
    abs0=abs(x-mu0)
    r0=rank(abs0)
    rank0=sign0*r0
    SRp=sum(rank0[rank0>0])
    n=n+1
    T=abs(SRp-n*(n+1)/4)
    Tcritical=Zalpha*sqrt(n*(n+1)*(2*n+1)/24)  
   }
   if(T>=Tcritical) {R=R+1} else {A=A+1}
   t=t+1
   SN[t]=n
 }
 pow[k]=R/simulation_runs
 ASN[k]=mean(SN)
 print(k)
}

cbind(mu,pow,ASN)

end_time=Sys.time()
end_time-start_time

