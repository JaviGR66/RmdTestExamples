# Limpiamos 
rm(list = ls())

#Librerias
library(ggplot2)
library(dplyr)
library(ISLR)
library(class)
library(caret)
library(ROCR)
library(pROC)

#Datos
dim(Caravan)
attach(Caravan)
summary(Purchase)

#Estandarizamos la liber�a
standardized.X=scale(Caravan[,-86])

# Creamos test y train
test =1:1000
train.X=standardized.X[-test ,] 
# Observaciones de 1-1000
test.X=standardized.X[test ,] 
train.Y=Purchase[-test]
test.Y=Purchase[test]

# Agregando los componentes con KNN
set.seed(1)
# Creamos la matriz de confusion (K=1)
knn.pred=knn(train.X,test.X, train.Y,k=1)
mean(test.Y!= knn.pred)

mean(test.Y!=" No")

# Matriz de confusion (K=1)
table(knn.pred,test.Y)

temporal<- table(knn.pred, test.Y)

# Porcentaje de acierto
cat("Porcentaje de acierto: ", (temporal[4]/(temporal[2] + temporal[4]) * 100), "%")

# Creamos la matriz de confusion (K=3)
knn.pred= knn(train.X, test.X, train.Y,k=3)
table(knn.pred, test.Y)

temporal<- table(knn.pred, test.Y)
# Porcentaje de acierto
cat("Porcentaje de acierto: ", (temporal[4]/(temporal[2] + temporal[4]) * 100), "%")

# Creamos la matriz de confusion (K=5)
knn.pred= knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

temporal<- table(knn.pred, test.Y)
# Porcentaje de acierto
cat("Porcentaje de acierto: ", (temporal[4]/(temporal[2] + temporal[4]) * 100), "%")

# Creamos la matriz de confusion (K=9)
knn.pred= knn(train.X, test.X, train.Y, k=9)
table(knn.pred, test.Y)

temporal<- table(knn.pred, test.Y)
# Porcentaje de acierto
cat("Porcentaje de acierto: ", (temporal[4]/(temporal[2] + temporal[4]) * 100), "%")

# Creamos la matriz de confusion (K=17)
knn.pred= knn(train.X, test.X, train.Y, k=17)
table(knn.pred, test.Y)

temporal<- table(knn.pred, test.Y)
# Porcentaje de acierto
cat("Porcentaje de acierto: ", (temporal[4]/(temporal[2] + temporal[4]) * 100), "%")

