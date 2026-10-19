#Práctica 2.3 Sindicato

library(tidyverse)  # Librería para análisis de datos
library(boot)       # Permite manejar datos e índices
library(car)        # Para poder visualizar los datos
library(QuantPsyc)  # Para uso de variantes y multivariantes
library(ggplot2)    # Para graficar con puntos

#Mantener nombres
attach(sindicato)

#mostrar los nombres
names(sindicato)

#tipo de dato
class(Años)
class(Satisfacción)

#contenido de la tabla
sindicato

#para estimar el coeficiente de correlación entre Años y Satisfacción
cor.test(Años, Satisfacción)

#Al tener un valor de 0.7113558 significa que la Satisfacción obtenida y los
#años de trabajo tienen una gran correlación positiva, por lo tanto
#los años influyen mucho en la satisfacción, donde ambos crecen en la 
#misma dirección, es decir a más años es mayor la satisfacción

#crear el modelo de regresión lineal
#primero la dependiente y luego la independiente
modelo1 = lm(Satisfacción ~ Años, data=sindicato)
summary(modelo1)

#sacamos la correlación de Pearson
#raíz cuadrada de R2
sqrt(0.4072)

#para aplicar nuestra ecuación, obtenemos los coeficientes
modelo1$coefficients

#para graficar el modelo
#etiqueta la línea de las Xs y de las Ys
grafica1 = ggplot(sindicato, aes(Años, Satisfacción))
grafica1 + geom_point()

#para graficar la línea de regresión
grafica1 + geom_point() + geom_smooth(method = "lm", colour = "Red")

#Predecir el índice de satisfacción de una persona que lleva 11 años militando al sindicato
newdata <- data.frame(Años = 11)
mod12 <- predict(modelo1, newdata)
print(mod12)

#Al hacer la predicción se puede observar que una persona con once años tendría una 
#Satisfacción de 8, por lo tanto significa que persona con más años afiliada 
#sentirá una mayor satisfacción en comparación a las personas con poco tiempo
#en el sindicato, por lo tanto si el sindicato busca ganar adeptos es necesario
#que analicen el por qué las personas con más años se sienten más satisfechas
#viendo comportamientos clave o haciendo encuestas para conocer más a 
#detalle las razones de su satisfacción y en base a eso
#traten de aplicar algunas estrategias con las personas recién llegadas o con menos
#años para que puedan sentirse más satisfechos con el trabajo hecho por el sindicato
#y así tengan en general una mayor satisfacción
