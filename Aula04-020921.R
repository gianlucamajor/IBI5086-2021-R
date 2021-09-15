##Exemplo apresentado em Aula: Dados de “Abundância Viral”
##Delineamento com 1 fator em 4 níveis: T1, T2, T3 e T4
Resp<-c(6.2,  4.8,  3.0,  5.6,  7.1,  4.8, 12.7, 11.3,  9.3,  9.3, 11.7,
        15.3,  7.0,  4.4, 3.8,  5.0,  5.5,  3.2,  8.3,  7.1, 11.7, 10.0,  8.5, 12.4)
Resp
Trat<-rep(c(1,2,3,4),each=6)
Trat
dados<-data.frame(cbind(Trat,Resp))
dados
str(dados)
dados$Trat = factor(dados$Trat)
str(dados)
attach(dados)

#Análise descritiva inicial
boxplot(Resp ~Trat, main="Boxplot")
#Atenção: construir o boxplot para dados com “pelo menos 10 observações” 
#Neste caso: r=6
#Alternativa: Gráfico de médias com sd (ou se)
library(ggplot2)
me<-tapply(Resp,Trat,mean)
sd<-tapply(Resp,Trat,sd)
dms<-data.frame(cbind(c("T1","T2","T3","T4"),me,sd))
dms$me<-as.numeric(me)
dms$sd<-as.numeric(sd)
str(dms)
ggplot(dms, aes(x=V1, y=me)) + 
  geom_errorbar(aes(ymin=me-sd/sqrt(6), ymax=me+sd/sqrt(6)), width=.1) +
  geom_point() + 
  ggtitle("Gráfico de médias (se)")

###Ajuste do Modelo ANOVA-DCA um fator em 4 níveis
# Usando o comando aov
mod1 <- aov(Resp ~ Trat, data = dados)
names(mod1)
summary(mod1)
anova(mod1)
model.matrix(mod1) #matrix X de planejamento
mod1$coefficients  #beta_hat
# Y_hat=X*beta_hat
#O R adota a parametrização Casela de Referência
#Interprete os coeficientes do modelo
#interprete descritivamente o valor F
#compare as somas de quadrados de Trat e Res
#QMRes estima qual parâmetro?
#Há evidência para efeito significante de Tratamento?
#Que suposições estão sendo assumidas?

# Diagnóstico - Análise de resíduos
par(mfrow=c(2,2))
plot(mod1)
par()

par(mfrow=c(2,2))
qqnorm(mod1$residuals,ylab="Residuos", main=NULL)
qqline(mod1$residuals)
title("Normalidade")

plot(seq(1:24), mod1$res, xlab="Índice dos dados", ylab="Resíduos",type="b")
abline(h=0)
title("Independência?")

plot(mod1$fit, mod1$res, xlab="Valores Ajustados", ylab="Resíduos")
abline(h=0)
title("Homocedasticidade?")

par()

# Teste de normalidade de Shapiro-Wilk
# H0: distribuição é normal
# p-valor > 0.05 não rejeita H0
shapiro.test(mod1$residuals)

#Teste da homogeneidade das variâncias
bartlett.test(Resp,Trat) 
#H0:Sigma_j=Sigma
#conclusão?

#Compações Múltiplas
summary(mod1)
model.matrix(mod1)  #parametrização casela de referência
mod1$coefficients

# Teste de Tukey para comparações múltiplas
fit.tu1 <- TukeyHSD(mod1)
fit.tu1
plot(fit.tu1) # Conclusão?

# Calculando o valor crítico "q" do método de Tukey
qtu <- qtukey(0.95,nmeans=4,df=24-4)
qtu

# Teste de Dunnett 
#para comparações múltiplas tendo um grupo de referência
install.packages('multcomp')

library(multcomp)
set.seed(12149890)
Dunnet1 <- glht(mod1, linfct=mcp(Trat="Dunnett"))
summary(Dunnet1) #compare com Tukey

plot(Resp~Trat, data=dat)
points(tapply(Resp,Trat,mean), cex =1.5, pch=18,col = "dark red")
#indicaçãop das médias dos grupos

#testes t bicaudais
pt.12 <-t.test(Resp[Trat=="1"],Resp[Trat=="2"],var.equal=TRUE,data=dat1)$p.value
pt.13 <-t.test(Resp[Trat=="1"],Resp[Trat=="3"],var.equal=TRUE,data=dat1)$p.value
pt.14 <-t.test(Resp[Trat=="1"],Resp[Trat=="4"],var.equal=TRUE,data=dat1)$p.value
pt.23 <-t.test(Resp[Trat=="2"],Resp[Trat=="3"],var.equal=TRUE,data=dat1)$p.value
pt.24 <-t.test(Resp[Trat=="2"],Resp[Trat=="4"],var.equal=TRUE,data=dat1)$p.value
pt.34 <-t.test(Resp[Trat=="3"],Resp[Trat=="4"],var.equal=TRUE,data=dat1)$p.value

results <- c(pt.12,pt.13,pt.14,pt.23,pt.24,pt.34)
results.o <- sort(results)  
results.o

# Obtendo valores-p ajustados 
p.adjust.methods
adjustb <- p.adjust(results.o,method="bonferroni")
cbind(results.o,adjustb) #p.ajustado=p.t * 6
results.o*6

adjustfdr <- p.adjust(results.o,method="fdr")
cbind(results.o,adjustfdr)

adjusth <- p.adjust(results.o,method="holm")
cbind(results.o,adjusth)

cbind(results.o,adjustb,adjusth,adjustfdr)

##Teste de Aleatorização
install.packages('lmPerm')

library(lmPerm)
anova(lmp(Resp ~ Trat, data = dados))

##No caso da ANOVA clássica o valor-p é calculado
#pela área à frente de F_obs:
plot(function(x) df(x,3,20), 0, 25, lwd=2)
abline(v=c(20.16),col = "red", lty = 3)
title("Distribuição F(3,20)")

##Ver recursos da biblioteca BHH2 para testes de aleatorização (Box, Hunter e Hunter, 1978)
##no caso de comparação de 2 grupos

################################################################
#Aplicação de ANOVA em genética Quantitativa
#Gerar dados genotípicos para 2 Locos independentes: L1 e L2 
# Usar a distribuição Binomial B(2,p); p=Probabilidade do alelo de risco A 
p1<-0.4
p2<-0.60
n<-100
set.seed(1475)
xg1<-rbinom(n,2,p1)
xg1
set.seed(1920)
xg2<-rbinom(n,2,p2)
xg2

#Gerar fenótipos quantitativos controlados pelo genótipo em L1 e L2 
# Gerar dados pela Mistura de Normais 
#L1: com efeito aditivo “a”  
#L2: com efeito aditivo e de dominância, “a” e “d”  

#Primeiro vamos gerar phen1 controlado por L1 
mi=165 #média do fenótipo quantitativo
a=15  #efeito aditivo do "gene" sobre o fenótipo
d=0   #efeito de dominância do "gene" sobre o fenótipo
s2=8  #variância do fenótipo
phen1<-c(NA,n) 
for(i in 1:n){ 
  set.seed(i)
  if (xg1[i] == 0) phen1[i]<- rnorm(1,mi-a,s2) 
  if (xg1[i] == 1) phen1[i]<- rnorm(1,mi+d,s2) 
  if (xg1[i] == 2) phen1[i]<- rnorm(1,mi+a,s2)
} 
dat1<-cbind(phen1,xg1) 
head(dat1) 
dat1
table(xg1)

hist(phen1) 
boxplot(phen1~xg1) 
title("Distribuição de phen1 por genótipo de L1")
round(tapply(phen1,xg1,mean),2) 
round(tapply(phen1,xg1,sd),2) 

perf1 <- matrix(0,1,3) 
perf1[1,1]<-mean(phen1[xg1=="0"]) 
perf1[1,2]<-mean(phen1[xg1=="1"]) 
perf1[1,3]<-mean(phen1[xg1=="2"]) 
perf1 
x <- matrix(c(0,1,2),1,3) 
plot(x,perf1, type="b",xlab="Genótipo1",ylab="Fenótipo1", axes=FALSE) 
title(main="Perfis médios - Efeitos genéticos (L1)",cex.main=1) 
axis(1,0:2) 
axis(2) 

# Ajuste de modelos lineares (ANOVA): Y=Xbeta+e 
#para estudar o efeito de L1 sobre phen1 
## Estimar os efeitos genéticos aditivo e de domin6ancia 
dat1<-data.frame(dat1)
str(dat1)
fit1.1 <- aov(phen1 ~ factor(xg1), data = dat1)
summary(fit1.1)
fit1.1
model.matrix(fit1.1) #matriz de planejamento X
fit1.1$coefficients  #beta_hat

fit.tu1 <- TukeyHSD(fit1.1)
fit.tu1
plot(fit.tu1) # Conclusão?

##estimativas dos parâmetros genéticos 
aditivo1<-fit1.1$coefficients[3]/2
aditivo1 

dominancia1<-fit1.1$coefficients[2]-fit1.1$coefficients[3]/2
dominancia1

#Função para calcular IC para combinações lineares de Beta
ci=function(fitout,C,df,alpha=0.05) {   
  beta=fitout$coefficients
  V=vcov(fitout)
  Cb=C%*%beta
  se=round(sqrt(diag(C%*%V%*%t(C))),4)
  tval=qt(1-alpha/2,df)
  low=round(Cb-tval*se,4)
  up=round(Cb+tval*se,4)
  m=cbind("C",Cb,se,low,up)
  dimnames(m)[[2]]=c(paste("C"),"estimate","se",paste(100*(1-alpha),"% Conf.",sep=""),"limits")
  m 
} 

#IC para o efeito aditivo
gl.1<-fit1.1$df.residual
Ci.a1<-matrix(c(0,0,1/2),nrow=1) 
ci(fit1.1,Ci.a1,gl.1) #IC para o efeito aditivo

#IC para o efeito de dominância
gl.1<-fit1.1$df.residual
Ci.d1<-matrix(c(0,1,-1/2),nrow=1) 
ci(fit1.1,Ci.d1,gl.1) #IC para o efeito de dominância

##Outra alternativa de ajuste para estimar efeitos genéticos
##Neste caso usamos um modelo de regressão e não de ANOVA
install.packages('car')
library(car) 
xa1<-Recode(xg1, "0=(-1);1=0;2=1") 
xd1<-Recode(xg1, "0=0;1=1;2=0") 
print(cbind(xg1,xa1,xd1)) 
fit1.2 <- glm(phen1 ~ xa1 + xd1, x=TRUE) 
summary(fit1.2) 
fit1.2$coefficients 

##Neste caso as variáveis preditoras xa1 e xd1 são usadas
##diretamente para fazer inferências sobre os parâmetros genéticos 



#Agora vamos gerar phen2 controlado por L2 
#com efeitos genéticos aditivo e de dominância 
mi=165
a=2
d=10
s2=8
phen2<-c(NA,n) 
for(i in 1:n){ 
  set.seed(i+10)
  if (xg2[i] == 0) phen2[i]<- rnorm(1,mi-a,s2) 
  if (xg2[i] == 1) phen2[i]<- rnorm(1,mi+d,s2) 
  if (xg2[i] == 2) phen2[i]<- rnorm(1,mi+a,s2)
} 
dat2<-cbind(phen2,xg2) 
head(dat2) 
dat2
table(xg2)

boxplot(phen2~xg2) 
title("Distribuição de phen2 por genótipo de L2")
round(tapply(phen2,xg2,mean),2) 
round(tapply(phen2,xg2,sd),2) 

perf2 <- matrix(0,1,3) 
perf2[1,1]<-mean(phen2[xg2=="0"]) 
perf2[1,2]<-mean(phen2[xg2=="1"]) 
perf2[1,3]<-mean(phen2[xg2=="2"]) 
perf2 
x <- matrix(c(0,1,2),1,3) 
plot(x,perf2, type="b",xlab="Genótipo2",ylab="Fenótipo2", axes=FALSE) 
title(main="Perfis médios - Efeitos genéticos (L2)",cex.main=1) 
axis(1,0:2) 
axis(2) 

# Ajuste de modelos lineares (ANOVA): Y=Xbeta+e 
#para estudar o efeito de L2 sobre phen2 
## Estimar os efeitos genéticos aditivo e de dominância 
dat2<-data.frame(dat2)
str(dat2)
fit2.1 <- aov(phen2 ~ factor(xg2), data = dat2)
summary(fit2.1)
fit2.1
model.matrix(fit2.1) #matriz de planejamento X
fit2.1$coefficients  #beta_hat

fit.tu2 <- TukeyHSD(fit2.1)
fit.tu2
plot(fit.tu2) # Conclusão?

##estimativas dos parâmetros genéticos 
aditivo2<-fit2.1$coefficients[3]/2
aditivo2 

dominancia2<-fit2.1$coefficients[2]-fit2.1$coefficients[3]/2
dominancia2

#IC para o efeito aditivo
gl.2<-fit2.1$df.residual
Ci.a2<-matrix(c(0,0,1/2),nrow=1) 
ci(fit2.1,Ci.a2,gl.2) #IC para o efeito aditivo

#IC para o efeito de dominância
gl.2<-fit2.1$df.residual
Ci.d2<-matrix(c(0,1,-1/2),nrow=1) 
ci(fit2.1,Ci.d2,gl.2) #IC para o efeito de dominância

##Outra alternativa de ajuste para estimar efeitos genéticos
##Neste caso usamos um modelo de regressão e não de ANOVA
library(car) 
xa2<-Recode(xg2, "0=(-1);1=0;2=1") 
xd2<-Recode(xg2, "0=0;1=1;2=0") 
print(cbind(xg2,xa2,xd2)) 
fit2.2 <- glm(phen2 ~ xa2 + xd2, x=TRUE) 
summary(fit2.2) 
fit2.2$coefficients 

##Neste caso as variáveis preditoras xa2 e xd2 são usadas
##diretamente para fazer inferências sobre os parâmetros genéticos 


###Ajuste do modelo ANOVA-DCA Fatorial 2x2
dados
F1<-rep(c(0,1),each=12)
F1
F2<-rep(rep(c(0,1),each=6),2)
F2
dados.i<-cbind(dados,F1,F2)
dados.i

#Gráfico de Perfis de Médias
#Permite a visualização do possível efeito de interação entre os fatores
with(dados.i, interaction.plot(F1, F2, Resp,main="Gráfico de interação"))
#o paralelismo dos perfis é uma indicação da NÃO existência de interação

mod2 <- aov(Resp ~ F1*F2, data = dados.i)
summary(mod2)

#Avalie a significância das fontes de variação sob estudo
#Há efeito de interação entre os fatores F1 e F2?
#Compare as tabelas ANOVA dos modelos ajustados mod1 e mod2
#Interprete os coeficientes estimados do modelo mod2
model.matrix(mod2) #matriz X de planejamento
mod2$coefficients 

# Diagnóstico - Análise de resíduos
par(mfrow=c(2,2))
plot(mod2)  #As suposições adotadas estão satisfeitas para análise destes dados?

##Ajuste do modelo ANOVA- Blocagem
##DABD sob esquema fatorial de tratamentos
dados.i
Bloc<-rep(c(1,2,3,4,5,6),4)
Bloc
dados.bl<-cbind(dados.i,Bloc)
dados.bl

mod3 <- aov(Resp ~ factor(Bloc)+F1*F2, data = dados.bl)
summary(mod3)
model.matrix(mod3)
mod3$coefficients 
model.tables(mod3, "means", se = TRUE)
#Compare o efeito dos fatores F1 e F2 sob os modelos DCA e DABC
#Neste caso houve ganho em precisão devido à blocagem?

# Diagnóstico - Análise de resíduos
par(mfrow=c(2,2))
plot(mod3)  
#As suposições adotadas estão satisfeitas para análise destes dados?
#Uma suposição importante no DABC
# é a aditividade entre os fatores Tratamento e Bloco
#Esse efeito de interação pode ser visualizado descritivamente por meio do 
#Gráfico de Perfis Individuais 
mod4<-aov(Resp ~ factor(Bloc)+Trat, data = dados.bl)
summary(mod4) #compare com o mod3
with(data.frame(dados.bl), interaction.plot(factor(Trat),factor(Bloc),Resp,main="Gráfico de interação"))

##Teste de Aleatorização - DABC
library(lmPerm)
anova(lmp(Resp ~ factor(Bloc)+Trat, data = dados.bl))
anova(lmp(Resp ~ factor(Bloc)+F1+F2+F1*F2, data = dados.bl))

