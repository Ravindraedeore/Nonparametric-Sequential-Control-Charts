##########################################################

################## SSRT Chart ############################

##########################################################

rm(list=ls())
start_time=Sys.time()
mu0=0
mu1=c(0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,3.00)
ASN0=10
ANSS0=512
simulation_runs=100000

a=0.87
b=3.361
ARL={}
ASN={}
for(k in 1:length(mu1))
{
  ARL1={}
  SN={}
  zj=(a+b)/2   ### Initial value of zj
  for(i in 1:simulation_runs)
  {
    SN1={}
    t=1
    zj=(a+b)/2 
    while(abs(zj)<b)
    {
      j=0
      x={}
      zj=(a+b)/2    ### To reset the value of zj
      while(abs(zj)>a && abs(zj)<b && j<5000)
      {
        x=c(x,rnorm(1,mu1[k],1))
        #x=c(x,rlaplace(1,mu1[k],1/sqrt(2)))
        #x=c(x,rlogis(1,mu1[k],sqrt(3)/pi))
        #x=c(x,runif(1,-sqrt(3)+mu1[k],sqrt(3)+mu1[k]))
        sign0=sign(x-mu0)
        abs0=abs(x-mu0)
        r0=rank(abs0)
        rank0=sign0*r0
        SRp=sum(rank0[rank0>0])
        
        j=j+1
        
        zj=(SRp-j*(j+1)/4)/sqrt(j*(j+1)*(2*j+1)/24)
        
      }
      SN1=c(SN1,j)
      t=t+1
      #print(round(c(i,j,a,zj,b),3))
    }
    ARL1[i]=t-1
    SN=c(SN,SN1)
    #print(c(k,i))
  }
  ARL[k]=mean(ARL1)
  ASN[k]=mean(SN)
  print(k)
}

ANOS=ARL*ASN
print(cbind(a,b,mu1,ASN,ARL,ANOS,simulation_runs))
print(cbind(mu1,ASN,ARL,ANOS))

end_time=Sys.time()
end_time-start_time