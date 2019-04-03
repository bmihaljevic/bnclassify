library(caret)
library(bnclassify)
data("voting")
fitControl <- trainControl( method = "repeatedcv", number = 10, repeats = 2)
V <- na.omit(voting)
data(car)
set.seed(0)
fit <- train(car[ , -ncol(car)], car[, 'class'],  method = "nbDiscrete", 
             trControl = fitControl )
fit  
