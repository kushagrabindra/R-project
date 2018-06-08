file<-read.csv("FinalKMeansDataset1.csv")
file
file<-file[-1]
file
x<-file[[1]]
x
y<-file[[2]]
y
train = data.frame(x,y)
train
plot(train,pch = 50)
model <- lm(y ~ x, train)
model
abline(model)
library(e1071)
model_svm <- svm(y ~ x , train)
model_svm
pred <- predict(model_svm, train)
pred
points(train$x, pred, col = "blue", pch=4)

error <- model$residuals
error
lm_error <- sqrt(mean(error^2)) # 3.832974
lm_error

error_2 <- train$y - pred
error_2
svm_error <- sqrt(mean(error_2^2))
svm_error

svm_tune <- tune(svm, y ~ x, data = train,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))
print(svm_tune)
best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, train) 

error_best_mod <- train$y - best_mod_pred 

# this value can be different on your computer
# because the tune method randomly shuffles the data
best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 

plot(svm_tune)

plot(train,pch=16)
points(train$x, best_mod_pred, col = "blue", pch=4)
