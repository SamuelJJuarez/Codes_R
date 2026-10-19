#Cargamos el conjunto de datos
attach(exportaciones)

#Mostramos los datos cargados
exportaciones

#Seleccionamos las columnas que usaremos para el cluster
X <- exportaciones[c(2,3)]

#Definimos el número de clusters (k)
k <- 3

#Ejecutamos el algoritmo k-medias
km.res <- kmeans(X, centers=k)

#Imprimimos los resultados
print(km.res)

#Calcular la inercia 
inercia <- km.res$tot.withinss
print(paste("Inercia total:", inercia))

#Observamos los valores proporcionados por el modelos
print(km.res$cluster)
print(km.res$centers)
print(km.res$totss)
print(km.res$withinss)
print(km.res$tot.withinss)
print(km.res$betweens)
print(km.res$size)
print(km.res$iter)
print(km.res$ifault)

#Visualizamos los clusters en un gráfico de dispersión
plot(X, col = km.res$cluster,
     main = paste("k-Medias con k=", k),
     xlab = "produccion",
     ylab = "exportaciones")

#Añadimos los centroides de cada cluster en el gráfico
points(km.res$centers, col = 1:k, pch = 8, cex = 2)