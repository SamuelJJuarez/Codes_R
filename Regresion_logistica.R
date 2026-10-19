#Regresión logística
library(tidyr)
library(mlogit)
library(dplyr)
library(ggplot2)

attach(curacion)

names(curacion)

curacion

class(Curado)
class(Intervencion)
class(Duracion)

str(curacion)

#Para elegir el campo Curado como factor
curacion$Curado<-as.factor(curacion$Curado)

#vemos los cambios en la estructura de Curado
str(curacion)

#para ver los valores de Curado
levels(curacion$Curado)

#crear modelo
modelo1= glm(Curado ~ Intervencion, data=curacion, family=binomial)

#para ver que datos tiene el modelo
names(modelo1)

#para ver los datos del modelo
summary(modelo1)

#para calcular el odds Ratio
exp(modelo1$coefficients)

#para ver la probabilidad de cada registro
modelo1$fitted.values

#para clasificar en base a la probabilidad
#se elige un punto de corte
curacion$valor<-as.numeric(modelo1$fitted.values>=0.5)

#para ver los nuevos valores
curacion

#para graficar
grafica1 = ggplot(curacion, aes(Intervencion,valor))
grafica1 + geom_point()

#Comparar duración con otro modelo
modelo1= glm(Curado ~ Intervencion+Duracion, data=curacion, family=binomial)
summary(modelo2)