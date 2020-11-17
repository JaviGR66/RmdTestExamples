library(C50)
library(gmodels)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(mlbench)
library(ROCR)
library(e1071)
library(caret)
library(dplyr)

# Extraemos la información y limpiamos
data <- read.csv2("german_credit.csv")
colSums(is.na(data))
str(data)

# Generamos una semilla aleatoria y seleccionamos un sample 
set.seed(123)
# Dataset formado por 1000 observaciones y 17 variables
train_sample <- sample(1000,800)

str(train_sample)

# Preparamos Train y Test
train <- data[train_sample,]
train$default <- as.factor(train$default)
test <- data[-train_sample,]

prop.table(table(train$default))
prop.table(table(test$default))

# Usamos el algoritmos C5.0 para el modelo
model <- C5.0(x=train[-17],train$default)
model

pred <- predict(model,test)
CrossTable(test$default,pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual','Predicción'))

# Usamos el algoritmos C5.0 para el modelo (añadimos los trials)
model1 <- C5.0(x=train[-17],train$default,trials = 10)
model1

pred1 <- predict(model1,test)
CrossTable(test$default,pred1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual','Predicción'))

# Creación del árbol
tree <- rpart(default ~ ., data=train)
rpart.plot(tree)