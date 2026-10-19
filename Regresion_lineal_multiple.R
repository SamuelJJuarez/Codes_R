# Regresión lineal múltiple
# Descargar las siguientes librerías
library(tidyverse)  # Librería para análisis de datos
library(boot)       # Permite manejar datos e índices
library(car)        # Para poder visualizar los datos
library(QuantPsyc)  # Para uso de variantes y multivariantes
library(ggplot2)    # Para graficar con puntos


# attach crea un entorno en la ruta de búsqueda
# (search path); en él copia los elementos de la lista
# o columnas del data frame. Permite manejar el archivo

attach(RMventas)

# Nombres de los valores de las columnas
names(RMventas)

# class regresa el tipo de datos
class(ventas)
class(Publicidad)
class(cantidad)


RMventas

# Para estimar el coeficiente de correlación entre la
# cantidad y las ventas
cor.test(cantidad, ventas)


# Crear el modelo de regresión lineal múltiple
modelo2 <- lm(ventas ~ Publicidad + cantidad, data = RMventas)
summary(modelo2)


# No se puede sacar la correlación de Pearson
# debido a que es un valor negativo
sqrt(0.255)


# Para aplicar nuestra ecuación, obtenemos los coeficientes
modelo2$coefficients


## Para graficar el modelo
# Etiqueta la línea de las Xs y de las Ys
grafica1 = ggplot(RMventas, aes(cantidad, ventas))
grafica1 + geom_point()


## Para graficar la línea de regresión
grafica1 + geom_point() +
  geom_smooth(method = "lm", colour = "red")