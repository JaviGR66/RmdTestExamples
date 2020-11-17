library(tm) #text mining package from R community, tm_map(), content_transformer()
library(SnowballC) #used for stemming, wordStem(), stemDocument()
library(RColorBrewer)
library(wordcloud) #wordcloud generator
library(e1071) #Naive Bayes
library(gmodels) #CrossTable()
library(caret) #ConfusionMatrix()
library(klaR)



file <- read.csv(file = "sms_spam.csv", header = TRUE, sep = ",", encoding="UTF-8")
file$type <- as.factor(file$type)

#Haremos la nube de spam y de ham por separados
#Mas adelante se mostrara una mezcla entre los dos
spam <- subset(file, type == "spam")
wordcloud(spam$text,scale = c (5.5,1.5), min.freq = 15, max.words = 60, rot.per = 0.2,colors = brewer.pal(8,"Dark2"))

ham <- subset(file, type == "ham")
wordcloud(ham$text,scale = c (5.5,0.5), min.freq = 15, max.words = 60, rot.per = 0.2,colors = brewer.pal(8,"Dark2"))

corpus <- VCorpus(VectorSource(file$text))


clean_corpus <- tm_map(corpus,content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus,removeNumbers)
clean_corpus <- tm_map(clean_corpus,removeWords,stopwords())
clean_corpus <- tm_map(clean_corpus,removePunctuation)
clean_corpus <- tm_map(clean_corpus,stripWhitespace)

#Imprime la nube con la mezcla de las palabras 
wordcloud(clean_corpus, min.freq = 50,random.order = FALSE)

dtm <- DocumentTermMatrix(clean_corpus)
#Separamos los datos con un 75% y un 25%
raw_train <- file[1:4180,]
raw_test <- file[4181:5574,]
dtm_train <- dtm[1:4180,]
dtm_test <- dtm[4181:5574,]

words <- findFreqTerms(dtm_train,5)
freq_train <- dtm_train[,words]
freq_test <- dtm_test[,words]

word <- function(x){
  x <- ifelse(x>0, "Yes","No")
}
train <- apply(freq_train,2,word)
test <- apply(freq_test,2,word)

bayes_model <- naiveBayes(train, raw_train$type,laplace = 1)


pred_bayes <- predict(bayes_model, test)
confusionMatrix(pred_bayes,raw_test$type,positive = "spam")