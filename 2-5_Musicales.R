# Librerías
library(ggplot2)

# Cargamos el conjunto de datos y los nombres de las columnas
attach(grupos_musicales)

# Mostramos los datos cargados
grupos_musicales

# Mostramos los nombres de las columnas
names(grupos_musicales)

# Mostramos el tipo de datos de las variables
class(`Cantidad de descargas`)
class(`Número de conciertos`)
class(Promoción)

# Mostramos la estructura del conjunto de datos
str(grupos_musicales)



# 1. Verificar la relación entre conciertos y descargas

# Calculamos el coeficiente de correlacion de Pearson
cor.test(`Número de conciertos`,`Cantidad de descargas`)


# El coeficiente de correlacion obtenido es r = 0.957, por lo tanto
# existe una relacion lineal positiva y fuerte entre el
# número de conciertos y la cantidad de descargas, lo que
# significa que, en estos datos, los grupos que realizan
# una mayor cantidad de conciertos tienden a tener también
# una mayor cantidad de descargas.
# Además, el valor p es 0.0106, menor que 0.05,
# por lo que la relacion resulta estadisticamente significativa
# al nivel de significancia del 5 %.



# 2. Uso de regresión lineal para predecir las descargas de Coldplay

# Crear una copia de los datos con nombres sencillos
grupos_limpios <- data.frame(
  descargas = grupos_musicales[["Cantidad de descargas"]],
  conciertos = grupos_musicales[["Número de conciertos"]],
  promocion = grupos_musicales[["Promoción"]]
)

# Mostrar los datos
grupos_limpios

# Creamos el modelo de regresion lineal simple
modelo1 <- lm(descargas ~ conciertos, data = grupos_limpios)
summary(modelo1)

# Obtenemos los coeficientes del modelo
modelo1$coefficients

# La ecuacion de regresión tiene la forma: Descargas = 4.038 + 0.1882(Conciertos)
# Por lo tanto por cada concierto adicional, el modelo estima un aumento
# de aproximadamente 0.1882 millones de descargas.
# Así que por lo tanto existe una relacion positiva entre ambas variables.

# Predicción de las descargas cuando se realizan 150 conciertos
prediccion <- predict(modelo1, newdata = data.frame(conciertos = 150))

prediccion

# Si ColdPlay realiza 150 conciertos al año, el modelo estima
# aproximadamente 32.27 millones de descargas.

# Gráfica de la regresión lineal
grafica1 <- ggplot(grupos_musicales,aes(`Número de conciertos`,`Cantidad de descargas`))
grafica1 + geom_point()

# Agregamos la línea de regresión
grafica1 + geom_point() + geom_smooth(method = "lm", colour = "red")



# 3. Uso de regresión logística para la probabilidad de promoción de Radiohead

# Promoción es la variable dependiente:
# 1 = se realizó promoción
# 0 = no se realizó promoción
#
# Variables independientes: Cantidad de descargas y Número de conciertos.

# Crear el modelo de regresión logística
modelo2 <- glm(promocion ~ descargas + conciertos, data = grupos_limpios, family = binomial)
summary(modelo2)

# Obtenemos los coeficientes del modelo
coeficientes <- modelo2$coefficients
coeficientes

# Calculamos la probabilidad de que se haga promoción a Radiohead considerando
# 22.5 millones de descargas y 100 conciertos
grave <- 161.76650 + -101.22136 * 22.5 + 21.81058 * 100
grave

# Calculamos la probabilidad de que se haga promoción
p <- 1 / (exp(-grave) + 1)
p

# Convertimos la probabilidad a porcentaje
p * 100

# El modelo calcula una probabilidad del 100 % para que se realice promoción 
# bajo las condiciones indicadas, considerando 22.5 millones de descargas y
# 100 conciertos, el modelo clasifica a Radiohead con una
# probabilidad segura de recibir promoción.



# 4. Agrupamiento de los grupos musicales para renovar contrato

# Seleccionamos las columnas numéricas que utilizaremos para realizar el cluster
# Cantidad de descargas y Número de conciertos
X <- grupos_musicales[c(2,3)]

# Estandarizamos los datos para evitar escalas diferentes
X <- scale(X)

# Definimos el número de clusters
k <- 2

# Ejecutamos el algoritmo k-medias
set.seed(123)

km.res <- kmeans(X, centers = k, nstart = 25)

# Imprimimos los resultados
print(km.res)

# Calcular la inercia
inercia <- km.res$tot.withinss
print(inercia)

# Observamos los valores proporcionados por el modelo
print(km.res$cluster)
print(km.res$centers)
print(km.res$totss)
print(km.res$withinss)
print(km.res$tot.withinss)
print(km.res$betweenss)
print(km.res$size)
print(km.res$iter)
print(km.res$ifault)

# Gráfica de los cluster
plot(X, col = km.res$cluster, main = paste("k-Medias con k =", k), xlab = "Cantidad de descargas",
     ylab = "Numero de conciertos")

# Añadimos los centroides de cada cluster en el gráfico
points(km.res$centers, col = 1:k, pch = 8, cex = 2)

# Al utilizar k = 2, los grupos se dividen en dos conjuntos
# de comportamiento: Un grupo concentra a los artistas con mayor cantidad de
# descargas y mayor número de conciertos, el otro grupo concentra a los artistas 
# con menor cantidad de descargas y menor número de conciertos
# ColdPlay y The Cranberries quedan en el grupo de mejor desempeño, mientras que
# Arctic Monkeys, Radiohead y Muse quedan en el grupo de desempeño relativamente menor.
# Por lo tanto, considerando esto, los grupos que presentan mejores
# indicadores para renovar contrato son ColdPlay y The Cranberries.