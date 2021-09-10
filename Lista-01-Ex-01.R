# Lista 01 - Ex 01
nReplica<-25 #numero de replicas
sigma2<-6  # variancia

#medias população 1 
mi11aa<-80
mi11Aa<-80
mi11AA<-80

mi12aa<-90
mi12Aa<-90
mi12AA<-90

#medias população 2 
mi21aa<-75
mi21Aa<-80
mi21AA<-90

mi22aa<-80
mi22Aa<-86
mi22AA<-97

#medias população 3  
mi31aa<-75
mi31Aa<-80
mi31AA<-85

mi32aa<-75
mi32Aa<-85
mi32AA<-100

#Populacao 1

aa<-rnorm(nReplica,mi11aa,sigma2)
Aa<-rnorm(nReplica,mi11Aa,sigma2)
AA<-rnorm(nReplica,mi11AA,sigma2)
t11 = stack(data.frame(cbind(aa,Aa,AA)))

aa<-rnorm(nReplica,mi12aa,sigma2)
Aa<-rnorm(nReplica,mi12Aa,sigma2)
AA<-rnorm(nReplica,mi12AA,sigma2)
t12<-stack(data.frame(cbind(aa,Aa,AA)))

t1<-rep(1:2,each=75)
d1<-rbind(t11,t12)
pop1<-data.frame(d1,t1)

summary(pop1)

#Populacao 2

aa<-rnorm(nReplica,mi21aa,sigma2)
Aa<-rnorm(nReplica,mi21Aa,sigma2)
AA<-rnorm(nReplica,mi21AA,sigma2)
t21 = stack(data.frame(cbind(aa,Aa,AA)))

aa<-rnorm(nReplica,mi22aa,sigma2)
Aa<-rnorm(nReplica,mi22Aa,sigma2)
AA<-rnorm(nReplica,mi22AA,sigma2)
t22<-stack(data.frame(cbind(aa,Aa,AA)))

t2<-rep(1:2,each=75)
d2<-rbind(t21,t22)
pop2<-data.frame(d2,t2)

summary(pop2)

#Populacao 3

aa<-rnorm(nReplica,mi31aa,sigma2)
Aa<-rnorm(nReplica,mi31Aa,sigma2)
AA<-rnorm(nReplica,mi31AA,sigma2)
t31 = stack(data.frame(cbind(aa,Aa,AA)))

aa<-rnorm(nReplica,mi32aa,sigma2)
Aa<-rnorm(nReplica,mi32Aa,sigma2)
AA<-rnorm(nReplica,mi32AA,sigma2)
t32<-stack(data.frame(cbind(aa,Aa,AA)))

t3<-rep(1:2,each=75)
d3<-rbind(t31,t32)
pop3<-data.frame(d3,t3)

summary(pop3)

