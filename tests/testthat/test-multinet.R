context("inference")   

test_that("plot", {
  bn <- multinet_cl(class = 'class', dataset = car)
  expect_output(plot(bn), "An ensemble of Bayesian network classifiers cannot be plotted")
}) 

test_that("Predict", { 
  bn <- multinet_cl(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  a <- compute_cp(x = bn, car)
  nparams(a)
})  

