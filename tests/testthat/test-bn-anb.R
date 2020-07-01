context("bn-anb")

data(iris)
iris$Sepal.Length<- factor(iris$Sepal.Length)
iris$Sepal.Width<- factor(iris$Sepal.Width)
iris$Petal.Length<- factor(iris$Petal.Length)
iris$Petal.Width<- factor(iris$Petal.Width)

test_that(" not a error with loglik", {
  bnanb<-bn_anb('Species', iris, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
})

test_that("error with bic",{
  expect_error( bn_anb('Species', iris, score = 'bic'), "length(dm) >= 3L is not TRUE" )
})

test_that("error with aic",{
  expect_error( bn_anb('Species', iris, score = 'bic'), "length(dm) >= 3L is not TRUE" )
})