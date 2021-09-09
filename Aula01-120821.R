install.packages('ggplot')
install.packages('Hmisc')
install.packages('UsingR')
install.packages("sciplot")
install.packages("tidyverse") # Adicionado para tentar resolver recode_factor - resolvido :) 
install.packages('reshape2')

library(Hmisc)
library(ggplot2)
library(UsingR)
library(psych)

library(forcats) # from tidyverse

data(normtemp)
head(normtemp)
str(normtemp)
attach(normtemp)
?normtemp

#Transformar °F  para °C : (32 °F − 32) × 5/9
tf<-normtemp$temperature
tc<-(tf-32)*5/9

temp<-data.frame(normtemp,tc)
head(temp)
temp

summary(tc)
mean(tc)
sd(tc)

m1<-mean(temp[1:65,4])
m2<-mean(temp[66:130,4])
dif<-m2-m1
sd1<-sd(temp[1:65,4])
sd2<-sd(temp[66:130,4])

library(psych)

describeBy(tc,factor(gender))

require(sciplot)
par(mfrow=c(1,2))
lineplot.CI(factor(temp$gender), tc, type="p", las=1,
            xlab="Sexo", ylab="Temperatura", main="Média e 2*EP",
            ylim=c(35.8,38),
            ci.fun= function(x) c(mean(x)-2*se(x), mean(x)+2*se(x)))


lineplot.CI(factor(temp$gender), tc, type="p", las=1,
            xlab="Sexo", ylab="Temperatura", main="Média e 2*DP",
            ylim=c(35.8,38),
            ci.fun= function(x) c(mean(x)-2*sd(x), mean(x)+2*sd(x)))

hist(tc, main="Temperatura Corporal")
temp<-data.frame(normtemp,tc)
ggplot(temp, aes(x = tc)) +
  geom_histogram(aes(color = factor(gender), fill = factor(gender)), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

ggplot(temp, aes(x = tc)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(factor(gender) ~ .)

temp_sex <- temp

temp_sex$gender <- recode_factor(factor(temp_sex$gender), '1' = 'Male', '2' = 'Female')

ggplot(temp_sex, aes(x = tc)) +
  geom_histogram(fill = "white", colour = "black", 
                 title="Histograma - Temperatura de acordo com o Sexo") +
  facet_grid(factor(gender) ~ .)

boxplot(tc ~ factor(gender), main="Boxplot - Temperatura de acordo com o Sexo")

plot(tc,hr)

install.packages("car")
library(car)
sex<-recode(gender,"1='1';2='0'")
sex
plot(hr~tc, pch=23, bg=c('red', 'blue')[factor(sex)])

#Lendo os dados Pulse

dados = read.table('Pulse.csv', head=T, sep=";", dec=",")

dados
head(dados)
str(dados)
attach(dados)
names(dados)
dados[,1:3]

summary(dados[,1:3])
table(Ran)
table(Ran,Sex)

mean(P1)
sd(P1)

library(psych)
describeBy(P1,factor(Ran))
describeBy(P2,factor(Ran))

#Convertendo o formato dos dados
#Pulse está em "wide format"
#P1 P2 ...
head(dados)
dat.wide<-dados

dat.wide
names(dat.wide)

library(reshape2)
#Convertendo do formato wide para o long
melt(olddata_wide, id.vars=c("subject", "sex"))
dat.long <- melt(dat.wide,id.vars=c("Ran","Fu","Sex","Altura", "Peso", "Ativ"))
head(dat.long)

#Convertendo do formato wide para long
dat.wide <- dcast(dat.long,  Ran + Fu + Sex + Altura + Peso + Ativ  ~ variable, value.var="value")
head(dat.wide)

dat.long
interaction.plot(dat.long$variable,dat.long$Ran,dat.long$value, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="Ran", 
                 ylab="Pulse", 
                 main="Perfis de Médias-Interação")

