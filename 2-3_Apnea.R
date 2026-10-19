#Mostramos los datos cargados
library(ggplot2)
library(tidyverse)
library(tidyr)
library(mlogit)
library(dplyr)

attach(apnea)
names(apnea)

apnea

class(Apnea)
class(PD)

str(apnea)

modelo1 = glm(Apnea ~ PD, data=apnea, family=binomial)
summary(modelo1)

grave <- -6.069 + 0.0427*140
grave

p <- 1 / (exp(-grave) + 1)
p

modelo1$fitted.values

grafica1 = ggplot(apnea, aes(Apnea,modelo1$fitted.values))
grafica1 + geom_point()