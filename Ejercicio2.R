#Ejercicio 2 - gráficar datos

#para definir un directorio por default
setwd("C:/Users/samue/OneDrive/Documentos/Universidad/Noveno semestre/Inteligencia Negocios II/EjerciciosR")

#extrae el documento de frecuencias.csv
frame <- read.csv("frecuencias.csv")

#imprime en la consola los valores de ciudad y valor
print(frame)

#asignar valores a una variable
x<-c(1,3,5,7,9)
y<-c(8,0,4,6,2)

print(x)
print(y)

#se asigna a variables los valores
ny <- 12327
la <- 17129
sf <- 19923

#se crea una tabla o matriz con los datos proporcionados
helados <-cbind(ny,la,sf)
is.matrix(helados)

#para imprimir la tabla creada
print(helados)

#obtener la media, mediana, moda y desviación estándar
mean(helados)
median(helados)
mode(helados)
sd(helados)

matplot(helados, type="b", pch= 15:17, col= 5:7)