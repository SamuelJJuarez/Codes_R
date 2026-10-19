# Regresión lineal simple
#Regresión simple
#descargar librerías
library(tidyverse) #análisis de datos
library(boot) #manejar datos e índices
library(cat) #poder visualizar los datos
library(ggplot2) # para gráficas con puntos
library(QuantPysc) # para uso de variantes y multivariantes

#attach crea un entorno en la ruta de búsqueda y en el copia los elementos de la lista
#o columnas del data frame (mantiene el nombre de las columnas)
attach(sales)

#nombres de los valores de las columnad
names(sales)

#class regresa el tipo de datos
class(ventas)
class(Publicidad)
sales

#para estimar el coeficiente de correlación entre las ventas y la publicidad
cor.test(Publicidad,ventas)

#crear el modelo de regresión lineal
#primero la dependiente y luego la independiente
modelo1 = lm(ventas ~ Publicidad, data=sales)
summary(modelo1)

#sacamos la correlación de Pearson
#raíz cuadrada de R2
sqrt(0.3313)

#para aplicar nuestra ecuación, obtenemos los coeficientes
modelo1$coefficients

#para graficar el modelo
#etiqueta la línea de las Xs y de las Ys
grafica1 = ggplot(sales, aes(Publicidad, ventas))
grafica1 + geom_point()

#para graficar la línea de regresión
grafica1 + geom_point() + geom_smooth(method = "lm", colour = "Red")

newdata <- data.frame(Publicidad = 1500)
mod12 <- predict(modelo1, newdata)
print(mod12)