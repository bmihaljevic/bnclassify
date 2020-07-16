context("bn-anb")

data(car)
data(iris)
iris$Sepal.Length<- factor(iris$Sepal.Length)
iris$Sepal.Width<- factor(iris$Sepal.Width)
iris$Petal.Length<- factor(iris$Petal.Length)
iris$Petal.Width<- factor(iris$Petal.Width)

test_that(" not a error with loglik", {
  bnanb<-bn_anb('class', car, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
})

test_that("negative mutual information",{ 
  a<-mutual_information("Species", "Sepal.Length", iris)
  b<-mutual_information(c("Species", "Sepal.Width"), "Sepal.Length", iris)
  expect_true(a>0 & b>0)
}) 

test_that("non-symmetric mutual information",{ 
  a<-mutual_information("Species", "Sepal.Length", iris)
  b<-mutual_information("Sepal.Length", "Species",  iris)
  expect_equal(a,b)
})

test_that("family score becomes negative after adding a parent",{ 
  a<-family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'loglik')
  b<-family_scores("Species", "Sepal.Length", iris, score = 'loglik')
  expect_true(a>=b)
}) 

test_that("family score with aic",{ 
  a<-family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'aic')
  b<-family_scores("Species", "Sepal.Length", iris, score = 'aic')
  expect_true(a>=b)
}) 

test_that("family score with bic",{ 
  a<-family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'bic')
  b<-family_scores("Species", "Sepal.Length", iris, score = 'bic')
  expect_true(a>=b)
}) 
