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

