library(BetaImplement)
library(testthat)

test_that(
  "compare the result by using lm function and betaImplement function (data=iris, subset=setosa)",
  {
    structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
    x<-BetaImplement(structure,iris)
    implement<-x$Sepal.Width[,1]

    data <- subset(iris, Species=='setosa')
    lmfunction <- lm(Sepal.Width~Sepal.Length,data)$coef

    expect_equal(implement,lmfunction)
  }
)


test_that("compare coefficients and sds to bnlearn", { 
    data(iris)
    structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
    x<-BetaImplement(structure,iris)
    coef <- x$Sepal.Length$coef
    sd <- x$Sepal.Length$sd
    
    library(bnlearn)
    a <- model2network(c("[Species][Sepal.Length|Species][Sepal.Width|Sepal.Length:Species][Petal.Length|Sepal.Length:Species][Petal.Width|Sepal.Length:Species]")) 
    fit <- bn.fit(a, iris)
    bncoef <- fit$Sepal.Length$coefficients
    bnsd <- fit$Sepal.Length$sd 
    expect_equal(coef, bncoef) 
    expect_equal(sd, bnsd)   
}) 
