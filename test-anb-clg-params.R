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
test_that("compare coefficients and sds to bnlearn:only categorical parent", {
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
test_that("compare coefficients and sds to bnlearn:one numerical parent+one categorical parent", {
  data(iris)
  structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  x<-BetaImplement(structure,iris)
  coef <-as.numeric(unlist(x$Petal.Width$coef))
  sd <- as.numeric(x$Petal.Width$sd)

  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  bncoef <-as.numeric( fit$Petal.Width$coefficients)
  bnsd <- as.numeric(fit$Petal.Width$sd)
  sd
  bnsd
  expect_equal(coef, bncoef)
  expect_equal(sd, bnsd)
})

#test4:
test_that("compare sds and coef to the result obtained from lm:one numerical parent",{
  sub<-subset(iris,Species=='setosa')
  l<-lm(Petal.Width~Sepal.Length,sub)
  lmsd<-as.numeric(sqrt(sum(l$residual^2)/(nrow(sub)-1-1)))
  lmcoef<-as.numeric(l$coefficients)

  data(iris)
  structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  x<-BetaImplement(structure,iris)
  sd <- as.numeric(x$Petal.Width$sd$setosa)
  coef <- as.numeric(x$Petal.Width$coef$setosa)

  expect_equal(sd, lmsd)
  expect_equal(coef,lmcoef)


})

#test5
test_that("compare sd and coef (obtained from lm) to bn",{
  a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Species:Petal.Length:Sepal.Length][Petal.Length|Species:Sepal.Length][Petal.Width|Species:Sepal.Length]"))
  fit <- bn.fit(a, iris)
  sdbn<-fit$Sepal.Width$sd
  coefbn<-fit$Sepal.Width$coefficients
  sdbn<-as.numeric(sdbn[1])
  coefbn<-as.numeric(coefbn[,1])

  sub<-subset(iris,Species=='setosa')
  l<-lm(Sepal.Width~Petal.Length+Sepal.Length,sub)
  lmsd<-as.numeric(sqrt(sum(l$residual^2)/(nrow(sub)-1-2)))
  lmcoef<-as.numeric(l$coefficients)

  expect_equal(lmsd,sdbn)
  expect_equal(coefbn,lmcoef)

}

)

