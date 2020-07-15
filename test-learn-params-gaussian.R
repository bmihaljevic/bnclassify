context('learn gaussian parameters')
library(bnlearn)
data(iris)

#result check
#test1:
test_that(
 "compare coef to lm",
 {
  structure<-tan_cl('Species',iris)
  x<-lp(structure,iris)
  implement<-as.numeric(unlist(x$params$Sepal.Width$coef$setosa))
  data <- subset(iris, Species=='setosa')
  lmfunction <- as.numeric(lm(Sepal.Width~Sepal.Length,data)$coef)
  expect_equal(implement,lmfunction)
 })

#test2:
test_that("compare coefficients and sds to bnlearn:only categorical parent", {
 structure<-tan_cl('Species',iris)
 x<-lp(structure,iris)
 coef <-as.numeric( x$params$Sepal.Length$coef)
 sd <- as.numeric(x$params$Sepal.Length$sd)

 a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
 fit <- bn.fit(a, iris)
 bncoef <-as.numeric( fit$Sepal.Length$coefficients)
 bnsd <- as.numeric(fit$Sepal.Length$sd)
 
 expect_equal(coef, bncoef)
 expect_equal(sd, bnsd)
})

#test3:
test_that("compare coefficients and sds to bnlearn:one numerical parent+one categorical parent", {
 structure<-tan_cl('Species',iris)
 x<-lp(structure,iris)
 coef <-as.numeric(unlist(x$params$Petal.Width$coef))
 sd <- as.numeric(x$params$Petal.Width$sd)
 
 a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
 fit <- bn.fit(a, iris)
 bncoef <-as.numeric( fit$Petal.Width$coefficients)
 bnsd <- as.numeric(fit$Petal.Width$sd)
 
 expect_equal(coef, bncoef, tolerance = 0.2)
 expect_equal(sd, bnsd, tolerance = 0.2)
})

#test4:
test_that("compare sds and coef to the result obtained from lm:one numerical parent",{
 sub<-subset(iris,Species=='setosa')
 l<-lm(Sepal.Width~Sepal.Length,sub)
 lmsd<-as.numeric(sqrt(sum(l$residual^2)/(nrow(sub)-1-1)))
 lmcoef<-as.numeric(l$coefficients)
 
 structure<-tan_cl('Species',iris)
 x<-lp(structure,iris)
 sd <- as.numeric(x$params$Sepal.Width$sd$setosa)
 coef <- as.numeric(x$params$Sepal.Width$coef$setosa)
 
 expect_equal(sd, lmsd)
 expect_equal(coef,lmcoef)
})

#dataset with multiple categorical
test_that('multiple categorical variable：parents list check',{
 season<-as.factor(sample(c("spring","summer",'autumn','winter'),150,replace = TRUE))
 data<-iris
 data[,6]<-season
 names(data)[6]<-'season'
 
 list<-Categorical_Numeric_list(c('season',"Sepal.Width"),data,'Species')
 expect_equal(list$factor,'season')
 expect_equal(list$numeric,'Sepal.Width')
})

test_that('multiple categorical variable：all possible combination',{
 season<-as.factor(sample(c("spring","summer",'autumn','winter'),150,replace = TRUE))
 data<-iris
 data[,6]<-season
 names(data)[6]<-'season'
 
 combination_name<-get_combination(c('season','Species'),data)
 expect_true(all(combination_name %in% c('spring','summer','autumn','winter','setosa','versicolor','virginica')))
 merged<-paste(combination_name[,1],combination_name[,2])
 
})

test_that('multiple categorical variable：result check',{
 season<-as.factor(sample(c("spring","summer",'autumn','winter'),150,replace = TRUE))
 data<-iris
 data[,6]<-season
 names(data)[6]<-'season'
 x<-fit_beta(data,c('season','Sepal.Width'),'Species','Sepal.Length')
 combination_name<-get_combination(c('season','Species'),data)
 merged<-paste(combination_name[,1],combination_name[,2])
 expect_length(x$coef,length(merged))
 expect_length(x$sd,length(merged))
 expect_equal(prod(dim(x$coef)),24) #2x12
 expect_equal(ncol(x$sd),12) #1x12
})