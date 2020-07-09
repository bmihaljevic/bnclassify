context('predict with gaussian variables')
library(e1071)


test_that('conditional probability of a single node',{
 nb <- bnc('nb','Species',iris)
 #posterior probability of the node Sepal.Length given class=setosa
 posprob <-dnorm(as.numeric(unlist(iris['Sepal.Length'])),
                 mean = nb$params$Sepal.Length$coef$setosa,
                 sd = nb$params$Sepal.Length$sd$setosa)#posterior probability of the node Sepal.Length given class=setosa
 ind_posprob <- get_conditionalProbability(iris,nb,'Sepal.Length',nb$params$Sepal.Length)
 expect_equal(posprob,ind_posprob[,1])
})

test_that('conditional probability of all node',{
 nb <- bnc('nb','Species',iris)
 posprob <-dnorm(as.numeric(unlist(iris['Sepal.Length'])),
                 mean = nb$params$Sepal.Length$coef$setosa,
                 sd = nb$params$Sepal.Length$sd$setosa)#posterior probability of the node Sepal.Length given class=setosa
 
 pos_prob <- local_predict(iris,nb,nb$params) #Posterior probability of each node
 expect(length(pos_prob),4)#4nodes
 expect(nrow(pos_prob[[1]]), 150)#150 instances
 expect(ncol(pos_prob[[1]]), 3)#3 labels
})

test_that('prior probability check',{
 prior_prob <- get_prior(iris,'Species')
 m <- naiveBayes(Species ~ ., data = iris)
 prior_prob_e1071 <- t(as.matrix(m$apriori/150))
 expect_equal(prior_prob,prior_prob_e1071)
})

test_that('normalization check',{
 nb <- bnc('nb','Species',iris)
 posprob <-dnorm(as.numeric(unlist(iris['Sepal.Length'])),
                 mean = nb$params$Sepal.Length$coef$setosa,
                 sd = nb$params$Sepal.Length$sd$setosa)#posterior probability of the node Sepal.Length given class=setosa
 ind_posprob <- get_conditionalProbability(iris,nb,'Sepal.Length',nb$params$Sepal.Length)
 norm<-normalization(ind_posprob)
 expect_equal(max(norm),1)
 expect_equal(min(norm),0)
})

test_that('prob=FALSE; Prob=TRUE',{
 nb <- bnc('nb','Species',iris)
 posprob <-dnorm(as.numeric(unlist(iris['Sepal.Length'])),
                 mean = nb$params$Sepal.Length$coef$setosa,
                 sd = nb$params$Sepal.Length$sd$setosa)#posterior probability of the node Sepal.Length given class=setosa

 x <- predict(nb,iris,prob=TRUE)
 predlabel <- names(x[which(x[1,]==max(x[1,]))])
 expect_equal(predlabel,as.character(predict(nb,iris,prob=FALSE)[1]))
})

test_that('check nb result',{
 nb<-nb('Species',iris)
 str<-lp(nb,iris)
 table_predict_acc_naive <- table(predict(str,iris),iris[,5])
 conf_matrix_predict <- as.numeric(table_predict_acc_naive)
 
 m <- naiveBayes(Species ~ ., data = iris)
 table_e1071<-table(predict(m, iris), iris[,5])
 conf_matrix_e1071 <-as.numeric(table_e1071)
 
 expect_equal(conf_matrix_predict,conf_matrix_e1071)     
})
