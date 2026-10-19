#1. Seleccionar variables
datos <- as.data.frame(exportaciones)

variables <- datos[, c("Producción", "Exportaciones")]

rownames(variables) <- as.character(datos$Año)
print(rownames(variables))

#2. Estandarizar las variables
datos_esc <- scale(variables)

#3. Calcular distancias
distancias <- dist(datos_esc, method = "euclidean")
distancias

#4. Generar el modelo jerárquico 
modelo <- hclust(distancias, method = "ward.D2")

#5. Mostrar el modelo
print(modelo)

#6. Generar el dedograma 
plot(modelo, main = "Agrupamiento jerárquico", xlab = "Año", ylab = "Altura")
rect.hclust(modelo, k = 2, border = "magenta")

# RESULTADOS DEL MODELO
#7. Identificar el grupo de cada año
grupos <- cutree(modelo, k = 2)
print(grupos)

#8. Cantidad de observaciones por grupo
table(grupos)

#9. Promedios (centroides) por grupo
centroides <- aggregate(
  variables,
  by = list(Grupo = grupos),
  FUN = mean
)
print(centroides)

#10. Resultados por observación
resultado <- data.frame(
  Año = datos$Año,
  Produccion = datos$Producción,
  Exportaciones = datos$Exportaciones,
  Grupo = grupos
)
print(resultado)

#11. Calcular variación dentro de cada grupo
withinss <- sapply(sort(unique(grupos)), function(g) {
  subgrupo <- datos_esc[grupos == g, , drop = FALSE]
  centro <- ColMeans(subgrupo)
  sum(sweep(subgrupo, 2, centro, "-")^2)
})
print(withinss)

#12. Suma de cuadrados total
totss <- sum(scale(datos_esc, center = TRUE,
                   scale = FALSE)^2)

#13. Suma de cuadrados dentro de grupos
tot.withinss <- sum(withinss)
tot.withinss

#14. Suma de cuadrados entre grupos
betweenss <- totss - tot.withinss
betweenss

#15. Porcentaje de variación entre grupos
porcentaje <- (betweenss / totss) * 100
porcentaje

#16. Resumen estadístico
cat("\nRESULTADOS DEL AGRUPAMIENTO\n")
cat("Total SS:", totss, "\n")
cat("Within SS:", tot.withinss, "\n")
cat("Between SS:", betwenss, "\n")
cat("Variación entre grupos", porcentaje, "$\n")