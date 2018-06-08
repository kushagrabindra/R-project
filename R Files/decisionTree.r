library(caret)
library(rpart.plot)
library(knitr)

new<-read.csv("xyz.csv",stringsAsFactors = FALSE)
summary(new)
kable(head(new))
new <- new[, c("Response", 'ASC', 'Airline', 'Timezone')]
mapply(function(x) sum(is.na(x)), new)
mapply(function(x) sum(!is.na(x)), new)
new <- new[complete.cases(new),]


smp_size <- floor(0.75 * nrow(new))
set.seed(2)
train_ind <- sample(seq_len(nrow(new)), size = smp_size)
train <- new[train_ind, ]
write.csv(train,"dtreetraining.csv",row.names = F)
test <- new[-train_ind, ]
write.csv(test,"dtreetesting.csv",row.names = F)



dtreeTrainingSet<-read.csv("dtreetraining.csv",stringsAsFactors = FALSE)
summary(dtreeTrainingSet)
kable(head(dtreeTrainingSet))
dtreeTrainingSet <- dtreeTrainingSet[, c("Response", 'ASC', 'Airline', 'Timezone')]
mapply(function(x) sum(is.na(x)), dtreeTrainingSet)
mapply(function(x) sum(!is.na(x)), dtreeTrainingSet)
dtreeTrainingSet <- dtreeTrainingSet[complete.cases(dtreeTrainingSet),]

dtreeTestingSet <-read.csv("dtreetesting.csv",stringsAsFactors = FALSE)

library(rpart)
fit <- rpart(Response ~ ASC + Airline + Timezone , data = dtreeTrainingSet, method = "class")
plot(fit)
text(fit)


set.seed(3)
library(rpart.plot)
rpart.plot(fit)
predicted= predict(fit,dtreeTestingSet)
predicted

