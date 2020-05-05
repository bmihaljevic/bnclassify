library(testthat)
library(bnlearn)

#test1:
test_that(
  "compare coef to lm",
  {
    structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
    x<-BetaImplement(structure,iris)
    implement<-as.numeric(unlist(x$Sepal.Width$coef$setosa))

    data <- subset(iris, Species=='setosa')
    lmfunction <- as.numeric(lm(Sepal.Width~Sepal.Length,data)$coef)

    expect_equal(implement,lmfunction)
  }
)

#test2:
test_that("compare coefficients and sds to bnlearn:categorical parent", {
  data(iris)
  structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  x<-BetaImplement(structure,iris)
  coef <-as.numeric( x$Sepal.Length$coef)
  sd <- as.numeric(x$Sepal.Length$sd)

  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  bncoef <-as.numeric( fit$Sepal.Length$coefficients)
  bnsd <- as.numeric(fit$Sepal.Length$sd)

  expect_equal(coef, bncoef)
  expect_equal(sd, bnsd)
})



#test3:
test_that("compare coefficients and sds to bnlearn:numerical parent+categorical parent", {
  data(iris)
  structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  x<-BetaImplement(structure,iris)
  coef <-as.numeric(unlist( x$Petal.Width$coef))
  sd <- as.numeric(x$Petal.Width$sd)
  sd
  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  bncoef <-as.numeric( fit$Petal.Width$coefficients)
  bnsd <- as.numeric(fit$Petal.Width$sd)
  bnsd
  expect_equal(coef, bncoef)
  expect_equal(sd, bnsd)
})

#test4:
test_that("compare sds to lm",{
  sub<-subset(iris,Species=='setosa')
  l<-lm(Petal.Width~Sepal.Length,sub)
  lmsd<-as.numeric(sd(l$residuals))

  data(iris)
  structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  x<-BetaImplement(structure,iris)
  sd <- as.numeric(x$Petal.Width$sd$setosa)

  expect_equal(sd, lmsd)
})

#test5
test_that("compare sd to bn by using the residuals obtained from fit function",{
  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  setosaRes<-fit$Petal.Width$residuals[1:50]
  sdSetosaFit<-as.numeric(sd(setosaRes))

  sub<-subset(iris,Species=='setosa')
  l<-lm(Petal.Width~Sepal.Length,sub)
  sdSetosaBetaImplement<-as.numeric(sd(l$residuals))

  expect_equal(sdSetosaBetaImplement,sdSetosaFit)
}

)

#test6
test_that("compare sd to bn by using the sd obtained from fit function",{
  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  sdbn<-fit$Petal.Width$sd
  sdSetosaFit<-as.numeric(sdbn[1])
  sdSetosaFit

  sub<-subset(iris,Species=='setosa')
  l<-lm(Petal.Width~Sepal.Length,sub)
  sdSetosaBetaImplement<-as.numeric(sd(l$residuals))
  sdSetosaBetaImplement

  expect_equal(sdSetosaBetaImplement,sdSetosaFit)
}

)
