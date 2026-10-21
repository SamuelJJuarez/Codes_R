# Practica 2-7 Estadios
library(ggplot2)

# Cargamos el conjunto de datos
attach(estadios_brasil_inv)

# Mostramos los datos cargados
estadios_brasil_inv

# Mostramos los nombres de las columnas
names(estadios_brasil_inv)

# Mostramos el tipo de datos
class(`Inversión`)
class(Capacidad)
class(Propietario)

# Mostramos la estructura de los datos
str(estadios_brasil_inv)


# 1. Calcular el coeficiente de correlación
# Para estimar el coeficiente de correlación entre la inversión
# y la capacidad de los estadios
cor.test(`Inversión`, Capacidad)

# Conclusión:
# El coeficiente de correlacion de Pearson obtenido es -0.0479
# por lo que existe una relación lineal negativa débil entre la inversión y la capacidad
# de los estadios
# el valor p es 0.4888, superior a 0.05 y por ello no existe evidencia estadística 
# para afirmar que hay una correlaci´´on lineal significativa
# entre la inversión y la capacidad por lo que una mayor inversión no se relaciona
# claramente con una mayor capacidad de los estadios.



# 2. Crear el modelo de regresión lineal
# Primero la variable dependiente y luego la independiente
modelo1 = lm(Capacidad ~ `Inversión`, data = estadios_brasil_inv)
summary(modelo1)

# Obtenemos los coeficientes del modelo
modelo1$coefficients

# Para graficar el modelo
grafica1 = ggplot(estadios_brasil_inv, aes(`Inversión`, Capacidad))
grafica1 + geom_point()

# Para graficar la línea de regresión
grafica1 + geom_point() + geom_smooth(method = "lm", colour = "Red")

# Conclusión:
# La ecuacion de regresion lineal obtenida es:
# Capacidad = 21898.60 - 6.4659 * Inversión
# La pendiente es negativa, lo que indica que el modelo
# estima una disminución aproximada de 6.4659 plazas por cada
# millón adicional de cruceiros invertidos pero la relación es débil y no es
# estadisticamente significativa
# el coeficiente R cuadrado es aproximadamente 0.0023,
# lo que significa que el modelo explica solamente el 0.23 %
# de la variabilidad observada en la capacidad por lo que, la inversión 
# por si sola no permite explicar la capacidad de los estadios



# 3. Predecir la capacidad para una inversión de 500 millones
newdata <- data.frame(`Inversión` = 500)
mod12 <- predict(modelo1, newdata)
print(mod12)

# Conclusión:
# Para una inversion de 500 millones de cruceiros,
# el modelo estima una capacidad aproximada de 18666 personas



# 4. Agrupamiento jerárquico de los estadios
# Seleccionar las variables numéricas
datos <- as.data.frame(estadios_brasil_inv)
variables <- datos[, c("Inversión", "Capacidad")]

# Identificar cada estadio en los resultados
rownames(variables) <- make.unique(as.character(datos$Estadio))

# Estandarizar las variables
datos_esc <- scale(variables)

# Calcular las distancias
distancias <- dist(datos_esc, method = "euclidean")
distancias

# Generar el modelo jerárquico
modelo <- hclust(distancias, method = "ward.D2")
print(modelo)

# Generar el dendrograma
plot(modelo,
     main = "Agrupamiento jerarquico de los estadios",
     xlab = "Estadios",
     ylab = "Altura")

# Dividir en 3 grupos
rect.hclust(modelo, k = 3, border = "magenta")

# Identificar el grupo de cada estadio
grupos <- cutree(modelo, k = 3)
print(grupos)

# Cantidad de estadios por grupo
table(grupos)

# Promedios de inversión y capacidad por grupo
centroides <- aggregate(
  variables,
  by = list(Grupo = grupos),
  FUN = mean
)
print(centroides)

# Resultados por estadio
resultado <- data.frame(
  Estadio = datos$Estadio,
  Propietario = datos$Propietario,
  Inversion = datos$`Inversión`,
  Capacidad = datos$Capacidad,
  Grupo = grupos
)
print(resultado)

# Análisis de los sectores de propietarios
# Gobierno, Estatal y Privado
# Se considera Gob. Estatal como Estatal, mientras que
# Gob. Municipal y Gob. Distrital se consideran Gobierno.
prop <- trimws(as.character(resultado$Propietario))
resultado$Sector <- ifelse(
  grepl("Privado|Private", prop, ignore.case = TRUE),
  "Privado",
  ifelse(
    grepl("Estatal|Estadual|State", prop, ignore.case = TRUE),
    "Estatal",
    "Gobierno"
  )
)

# Mostrar la cantidad total de estadios por sector
table(resultado$Sector)

# Mostrar la cantidad de estadios de cada sector
# dentro de cada grupo jerárquico
table(resultado$Grupo, resultado$Sector)

# Graficar la cantidad total de estadios por sector
barplot(
  table(resultado$Sector),
  main = "Cantidad de estadios por sector propietario",
  xlab = "Sector propietario",
  ylab = "Numero de estadios"
)

# Conclusión:
# Al analizar la cantidad total de estadios por sector, se
# observa que Gobierno cuenta con 100 estadios, Privado con 80
# y Estatal con 31, por lo tanto, Gobierno es el sector con mayor cantidad de
# estadios y se puede aplicar la iniciativa de apoyo para el financiamiento de 
# construcción de más estadios

