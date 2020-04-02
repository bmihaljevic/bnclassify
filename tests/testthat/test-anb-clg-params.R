context("filename: test_anb-clg-params")
test_that("filename is test_anb-clg-params", {
  #structure<-tan_cl('Species',as.data.frame(lapply(iris,as.factor)))
  structure<-nb('Species',as.data.frame(lapply(iris,as.factor)))
  plot(structure)
  result<-lp_implement(structure,iris)
  print(result)
})
