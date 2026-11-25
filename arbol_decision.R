# Instalar paquetes (ejecutar solo si aún no están instalados)
install.packages("titanic")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")

# Cargar librerías
library(rpart)
library(rpart.plot)
# cargar la librería de los datos
library(titanic)

# Cargar los datos
data("titanic_train")

# Ver los primeros datos
head(titanic_train)

# Modelar con árbol de decisión y predecir sobre la clase
arbol <- rpart(
  formula = Survived ~ Sex + Age,
  data = titanic_train,
  method = "class"
)

# Graficar el árbol
rpart.plot(arbol)

# Predecir con el árbol
pred_arbol <- predict(arbol, type = "class")
pred_arbol

# Agregar la columna pred_arbol a los datos
titanic_pred <- cbind(titanic_train, pred_arbol)

# Predicción para un pasajero masculino de 4 años de edad
predict(
  object = arbol,
  newdata = data.frame(Age = 4, Sex = "male"),
  type = "class"
)
