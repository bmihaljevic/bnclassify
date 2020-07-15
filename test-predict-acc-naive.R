library(testthat)
library(e1071)

test_that(
  'compare the cofusion matrix to result obtained from e1071 package ',
  {
structure<-nb('Species',as.data.frame(lapply(iris,as.factor)))
gaussianParams<-BetaImplement(structure,iris)
acc<-predict_acc_naive(iris,structure,gaussianParams)
table_predict_acc_naive <- table(acc,iris[,5])
conf_matrix_predict <- as.numeric(table_predict_acc_naive)

m <- naiveBayes(Species ~ ., data = iris)
table_e1071<-table(predict(m, iris), iris[,5])
conf_matrix_e1071 <-as.numeric(table_e1071)

expect_equal(conf_matrix_predict,conf_matrix_e1071)
}
)

test_that(
  'compare the accuracy to result obtained from bnclassify package ',
  {
data<-iris
data[,1]<-as.factor(data[,1])
data[,2]<-as.factor(data[,2])
data[,3]<-as.factor(data[,3])
data[,4]<-as.factor(data[,4])
nb <- bnc('nb', 'Species', data, smooth = 1)
p <- predict(nb, data,prob=FALSE)
bn_acc <- accuracy(p, data$Species)

structure<-nb('Species',as.data.frame(lapply(iris,as.factor)))
gaussianParams<-BetaImplement(structure,iris)
acc<-predict_acc_naive(iris,structure,gaussianParams,prob=FALSE)
pred_acc <- accuracy(acc,data$Species)

expect_equal(pred_acc,bn_acc)

}
)

