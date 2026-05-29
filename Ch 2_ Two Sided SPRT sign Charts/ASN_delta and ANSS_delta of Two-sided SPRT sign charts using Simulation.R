rm(list=ls())
p0=0.5      ##In-Control value
delta1 =  0.3
p1p=p0 + delta1     ##Upper shift
p1m=p0 - delta1    ##Lower Shift

#####Compute Gamma
r1p=-log(2-2*p1p)
r2p=log(p1p/(1-p1p))
r1m=-log(2-2*p1m)
r2m=log(p1m/(1-p1m))
gammap=r1p/r2p
gammam=r1m/r2m


g_U=g_L=-1.323
h_U=h_L=4.797


###########ASN and ANSS in same progr
start_time=Sys.time()
ASN1={}
ASN2={}
ANSS={}
P=c(0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95)
#P=c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50)
for(l in 1:length(P))
{
SN1={}
SN11={}
ANSS2={}
for(i in 1:10000)
{
 SN2={}
 j=0
 Tu = 0
 Tl = 0
 while(Tu < h_U && Tl < h_L)
 {
  Tu = 0
  Tl = 0
  k = 0
  while((Tu < h_U && Tl < h_L) && (Tu > g_U || Tl > g_L))
  {
   u = rbinom(1,1,P[l])
   k = k + 1
   if(Tu<g_U) {Tu=Tu}  else {Tu = Tu + u - gammap}
   if(Tl<g_L) {Tl=Tl} else  {Tl = Tl - u + gammam}
  }
  j=j+1
  SN2=c(SN2,k)
 }
 SN1[i]= mean(SN2)
 SN11 = c(SN11,SN2)
 ANSS2[i] = j
}
ANSS[l]=mean(ANSS2)
ASN1[l]= mean(SN1)
ASN2[l]= mean(SN11)
print(l)
}

ANOS1=ANSS*ASN1
ANOS2=ANSS*ASN2
end_time=Sys.time()
end_time-start_time
B1=cbind(P,ANSS,ANOS1,ANOS2)
B1