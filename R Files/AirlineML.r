library(RTextTools)
library(e1071)
library(dplyr) 
library(tidytext) 
AirlineML<-read.csv("sentiment.csv",stringsAsFactors = F)
AirlineML

cr<-AirlineML[[2]]
cr

t<-AirlineML[[11]]
t

removeSpecialChars <- function(t) gsub("[^a-zA-Z0-9 ]", " ", t)
t <- sapply(t, removeSpecialChars)
t<-sapply(t,tolower)
fix.contractions<-function(t)
{
  t <- gsub("won't", "will not", t)
  t <- gsub("can't", "can not", t)
  t <- gsub("n't", " not", t)
  t <- gsub("'ll", " will", t)
  t <- gsub("'re", " are", t)
  t <- gsub("'ve", " have", t)
  t <- gsub("'m", " am", t)
  t <- gsub("'d", " would", t)
  t <- gsub("'s", "", t)
  return(t)}

AirlineML$text<-t
AirlineMLrefined <- AirlineML %>% 
  select(Response = airline_sentiment,tweets = text)
AirlineMLrefined

write.csv(AirlineMLrefined,"AirlineMLDataset.csv",row.names = F)

application<-read.csv("AirlineMLDataset.csv")


smp_size <- floor(0.75 * nrow(application))

set.seed(123)
train_ind <- sample(seq_len(nrow(application)), size = smp_size)
train <- application[train_ind, ]
write.csv(train,"training.csv",row.names = F)
test <- application[-train_ind, ]
write.csv(test,"testing.csv",row.names = F)
matrix= create_matrix(train$tweets, language="english", 
                      removeStopwords=FALSE, removeNumbers=FALSE, 
                      stemWords=FALSE) 
mat = as.matrix(matrix)
classifier = naiveBayes(mat, as.factor(train$Response))

matrix1= create_matrix(test$Response, language="english", 
                      removeStopwords=TRUE, removeNumbers=FALSE, 
                      stemWords=FALSE) 
mat1=as.matrix(matrix1)
predicted = predict(classifier, mat1[1:10,])
predicted
Ta<-table(test[1:10,1], predicted)
Re<-recall_accuracy(test[1:10,1], predicted)
Re
Ta