# Cargamos el conjunto de datos
attach(frutas)
frutas

# Mostramos los nombres de las columnas
names(frutas)

# Mostramos la estructura de los datos
str(frutas)

# Seleccionamos las columnas para realizar el cluster: peso y dulzura
X <- frutas[c(2,3)]

# Estandarizamos los datos para que ambas variables tengan 
# valores comparables al calcular las distancias.
X <- scale(X)
X


# MODELO 1: k-means con 3 clusteres
# Definimos el numero de clusters
k <- 3

# Fijamos una semilla para reproducir los resultados
set.seed(123)

# Ejecutamos el algoritmo k-means
km3 <- kmeans(X, centers=k, nstart=25)
km3

# Calcular la inercia
inercia3 <- km3$tot.withinss
print(paste("Inercia total para k=3:", round(inercia3, 4)))

# Observamos los valores proporcionados por el modelo
print(km3$cluster)
print(km3$centers)
print(km3$totss)
print(km3$withinss)
print(km3$tot.withinss)
print(km3$betweenss)
print(km3$size)
print(km3$iter)
print(km3$ifault)

# Mostramos a que cluster pertenece cada fruta
resultado3 <- data.frame(Fruta = frutas[[1]], Cluster = km3$cluster)
resultado3

# Visualizamos los clusters en un gráfico de dispersión
plot(X,
     col = km3$cluster,
     pch = 19,
     main = paste("k-Medias con k =", k),
     xlab = "Peso",
     ylab = "Dulzura")

# Anadimos los centroides de cada cluster en el gráfico
points(km3$centers, col = 1:k, pch = 8, cex = 2)

# Conclusión de este modelo:
# Se busca distinguir tres comportamientos:
# 1. Frutas de poco peso y dulzura alta: Cereza, Uva y Fresa.
# 2. Frutas de peso intermedio y dulzura relativamente baja: Durazno, Kiwi y Melon.
# 3. Frutas de mayor peso y dulzura media o alta: Manzana, Platano, Pera y Mango.
# Estos grupos permiten distinguir tres perfiles de frutas
# y facilita interpretar las diferencias entre ellas



# MODELO 2: k-means con 4 clusters
# Definimos el numero de clusters
k <- 4

# Fijamos una semilla para reproducir los resultados
set.seed(123)

# Ejecutamos el algoritmo k-means
km4 <- kmeans(X, centers=k, nstart=25)
km4

# Calcular la inercia
inercia4 <- km4$tot.withinss
print(paste("Inercia total para k=4:", round(inercia4, 4)))

# Observamos los valores proporcionados por el modelo
print(km4$cluster)
print(km4$centers)
print(km4$totss)
print(km4$withinss)
print(km4$tot.withinss)
print(km4$betweenss)
print(km4$size)
print(km4$iter)
print(km4$ifault)

# Mostramos a que cluster pertenece cada fruta
resultado4 <- data.frame(Fruta = frutas[[1]], Cluster = km4$cluster)
resultado4

# Visualizamos los clusters en un gráfico de dispersión
plot(X,
     col = km4$cluster,
     pch = 19,
     main = paste("k-Medias con k =", k),
     xlab = "Peso",
     ylab = "Dulzura")

# Anadimos los centroides de cada cluster en el gráfico
points(km4$centers, col = 1:k, pch = 8, cex = 2)

# Conclusión de este modelo:
# Se obtiene una agrupacion mas detallada que con k=3, sin embargo, 
# con estos datos se separa el Plátano en un cluster individual, 
# mientras que las demás frutas se distribuyen en grupos de tres;
# esto puede representar una división demasiado detallada para solamente 10 frutas,
# aunque la inercia disminuye, tener más clusters no significa ser un mejor modelo.



# MODELO 3: k-means con 2 clusters
# Definimos el numero de clusters
k <- 2

# Fijamos una semilla para reproducir los resultados
set.seed(123)

# Ejecutamos el algoritmo k-means
km2 <- kmeans(X, centers=k, nstart=25)
km2

# Calcular la inercia
inercia2 <- km2$tot.withinss
print(paste("Inercia total para k=2:", round(inercia2, 4)))

# Observamos los valores proporcionados por el modelo
print(km2$cluster)
print(km2$centers)
print(km2$totss)
print(km2$withinss)
print(km2$tot.withinss)
print(km2$betweenss)
print(km2$size)
print(km2$iter)
print(km2$ifault)

# Mostramos a que cluster pertenece cada fruta
resultado2 <- data.frame(Fruta = frutas[[1]], Cluster = km2$cluster)
resultado2

# Visualizamos los clusters en un gráfico de dispersión
plot(X,
     col = km2$cluster,
     pch = 19,
     main = paste("k-Medias con k =", k),
     xlab = "Peso",
     ylab = "Dulzura")

# Anadimos los centroides de cada cluster en el gráfico
points(km2$centers, col = 1:k, pch = 8, cex = 2)


# Conclusión de este modelo:
# Se distinguen principalmente las frutas de poco peso
# y dulzura alta frente al resto de las frutas, por lo que
# la agrupacion es sencilla, pero junta frutas con pesos
# y niveles de dulzura bastante diferentes en un mismo grupo, 
# por ello, proporciona menos detalle que el modelo con tres clusters



# Comparación de los 3 modelos
# Comparamos la inercia y los tamaños de los clusters
comparacion <- data.frame(
  Clusters = c(2, 3, 4),
  Inercia = c(
    km2$tot.withinss,
    km3$tot.withinss,
    km4$tot.withinss
  ),
  Variacion_entre_clusters = c(
    km2$betweenss,
    km3$betweenss,
    km4$betweenss
  ),
  Cluster_mas_pequeno = c(
    min(km2$size),
    min(km3$size),
    min(km4$size)
  ),
  Cluster_mas_grande = c(
    max(km2$size),
    max(km3$size),
    max(km4$size)
  )
)
comparacion

# Graficamos la inercia para comparar los modelos
plot(comparacion$Clusters,
     comparacion$Inercia,
     type = "b",
     pch = 19,
     main = "Comparacion de la inercia de K-medias",
     xlab = "Numero de clusters",
     ylab = "Inercia total")

# Explicación del modelo más adecuado:
# La inercia disminuye al aumentar el número de clusters,
# porque los grupos se vuelven más pequenos y homogéneos, pero no se debe elegir 
# un modelo solamente por tener la inercia mas baja.
# Con estos datos, k=2 proporciona una agrupación general, mientras que k=4 
# puede dividir demasiado las observaciones y dejar una fruta en un cluster individual.
# Se considera que k=3 ofrece el mejor equilibrio entre distinguir los perfiles 
# de las frutas y mantener grupos suficientemente representativos para tomar una decisión.
