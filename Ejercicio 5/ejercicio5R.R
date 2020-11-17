library(caret)
library(nnet)
library(lattice)
library(ggplot2)
library(ROCR)

# Leemos el csv 
data <- read.csv2("Titanic.csv",sep = ";",stringsAsFactors = FALSE,header=TRUE)
# Lo limpiamos
data <- data[!apply(is.na(data) | data == "", 1, all), ]

#Separamos el contenido del csv en 75% train y 25% test
size_ <- floor(0.75*nrow(data))
size_

#Generamos valores aleatorios
set.seed(5)
train_ind <- sample(seq_len(nrow(data)),size=size_)
train <- data[train_ind,]
test <- data[-train_ind,]
model_1 <- glm(Survived ~ Pclass, family=binomial(link='logit'),data=train)
summary(model_1)

#Hacemos la prediccion del modelo
predict_1 <- predict(model_1, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pred_1 <- ROCR::prediction(predict_1, test$Survived)
perf_1 <- performance(pred_1, measure = "tpr", x.measure = "fpr")
plot(perf_1)
auc_1 <- performance(pred_1, measure = "auc")
auc_1 <- auc_1@y.values[[1]]
auc_1

confusionMatrix(table(ifelse(predict_1 < 0.5, 0, 1), test$Survived), dnn = c("predicted", "actual"))

#Creamos el segundo modelo
model_2 <- glm(Survived ~., family=binomial(link='logit'),data=train)
summary(model_2)

#Hacemos la prediccion del modelo
predict_2 <- predict(model_2, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pred_2 <- ROCR::prediction(predict_2, test$Survived)
perf_2 <- performance(pred_2, measure = "tpr", x.measure = "fpr")
plot(perf_2)
auc_2 <- performance(pred_2, measure = "auc")
auc_2 <- auc_2@y.values[[1]]
auc_2

confusionMatrix(table(ifelse(predict_2 < 0.5, 0, 1), test$Survived), dnn = c("predicted", "actual"))