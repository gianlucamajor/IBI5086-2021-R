install.packages('ggplot')
install.packages('Hmisc')
install.packages('UsingR')
install.packages("sciplot")
install.packages("tidyverse") # Adicionado para tentar resolver recode_factor - resolvido :) 

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

