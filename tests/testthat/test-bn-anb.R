context("bn-anb")

data(car)
data(iris)
library(discretization)
iris1<-mdlp(iris)$Disc.data
iris$Sepal.Length<- factor(iris$Sepal.Length)
iris$Sepal.Width<- factor(iris$Sepal.Width)
iris$Petal.Length<- factor(iris$Petal.Length)
iris$Petal.Width<- factor(iris$Petal.Width)

test_that(" not a error with loglik", {
  bnanb<-bn_anb('class', car, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
  expect_true(narcs(bnanb) > 6)
})

test_that(" not a error with loglik", {
  col_names <- names(iris1)
  iris1[,col_names] <- lapply(iris1[,col_names] , factor)
  bnanb<- bn_anb('Species', iris1, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
  expect_true(narcs(bnanb) > 4)
})

test_that("negative mutual information",{ 
  a<-mutual_information("class", "safety", car)
  b<-mutual_information(c("class", "lug_boot"), "safety", car)
  expect_true(a>0 & b>0)
}) 

test_that("non-symmetric mutual information",{ 
  a<-mutual_information("class", "safety", car)
  b<-mutual_information("safety", "class", car)
  expect_equal(a,b)
})

test_that("family score becomes negative after adding a parent",{ 
  a<-mutual_information(c("class", "lug_boot"), "safety", car)
  b<-mutual_information("class", "safety", car)
  expect_true(a>=b)
}) 

test_that("entropy becomes negative after adding a parent",{ 
  freqs1 <- extract_ctgt(c(c("class", "lug_boot"), "safety"), car)
  freqs2 <- extract_ctgt(c("class", "safety"), car)
  entrpy1 <- entropy::entropy(freqs1, method = "ML", unit = "log", verbose = F)
  entrpy2 <- entropy::entropy(freqs2, method = "ML", unit = "log", verbose = F)
  expect_true(entrpy1>=entrpy2)
}) 


test_that("family score becomes negative after adding a parent",{ 
  a<-family_scores(c("class", "lug_boot"), "safety", car, score = 'loglik')
  b<-family_scores("class", "safety", car, score = 'loglik')
  expect_true(a>=b)
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

test_that("entropy becomes negative after adding a parent",{ 
  freqs1 <- extract_ctgt(c(c("Species", "Sepal.Width"), "Sepal.Length"), iris)
  freqs2 <- extract_ctgt(c("Species", "Sepal.Length"), iris)
  entrpy1 <- entropy::entropy(freqs1, method = "ML", unit = "log", verbose = F)
  entrpy2 <- entropy::entropy(freqs2, method = "ML", unit = "log", verbose = F)
  expect_true(entrpy1>=entrpy2)
}) 

test_that("family score with aic",{ 
  a<-family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'aic')
  b<-family_scores("Species", "Sepal.Length", iris, score = 'aic')
  expect_true(a<=b)
}) 

test_that("family score with bic",{ 
  a<-family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'bic')
  b<-family_scores("Species", "Sepal.Length", iris, score = 'bic')
  expect_true(a<=b)
}) 
