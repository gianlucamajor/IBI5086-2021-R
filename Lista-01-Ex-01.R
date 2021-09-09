#Gerando dados
#Exemplo de comandos no R para gerar dados de P2:

r2<-25 #numero de replicas
sigma2<-6  # variancia

# medias 
mi201<-75
mi202<-80
mi211<-80
mi212<-86
mi221<-90
mi222<-97

y201<-rnorm(r2,mi201,sigma2)
y202<-rnorm(r2,mi202,sigma2)
y211<-rnorm(r2,mi211,sigma2)
y212<-rnorm(r2,mi212,sigma2)
y221<-rnorm(r2,mi221,sigma2)
y222<-rnorm(r2,mi222,sigma2)

y2<-stack(data.frame(cbind(y201,y202,y211,y212,y221,y222)))

fg2<-rep(0:2,each=50)
ft2<-rep(rep(1:2,each=25),3)
dat<-data.frame(cbind(y2,fg2,ft2))
dat

nReplica<-25 #numero de replicas
sigma2<-6  # variancia

#medias 
mi11aa<-80
mi11Aa<-80
mi11AA<-80

mi12aa<-90
mi12Aa<-90
mi12AA<-90

aa<-rnorm(nReplica,mi11aa,sigma2)
Aa<-rnorm(nReplica,mi11Aa,sigma2)
AA<-rnorm(nReplica,mi11AA,sigma2)

t1 = stack(data.frame(cbind(aa,Aa,AA)))


aa<-rnorm(nReplica,mi12aa,sigma2)
Aa<-rnorm(nReplica,mi12Aa,sigma2)
AA<-rnorm(nReplica,mi12AA,sigma2)

t2<-stack(data.frame(cbind(aa,Aa,AA)))

t<-rep(1:2,each=75)
d<-rbind(t1,t2)
dados<-data.frame(d,t)

summary(dados)
