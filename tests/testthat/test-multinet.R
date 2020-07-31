context("inference")   

test_that("plot", {
  bn <- multinet_cl(class = 'class', dataset = car)
  expect_output(plot(bn), "An ensemble of Bayesian network classifiers cannot be plotted")
}) 

test_that("Predict", { 
  bn <- multinet_cl(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  a <- compute_cp(x = bn, car)
})    

test_that("Accuracy", { 
  bn <- multinet_cl(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1) 
  p_bn <- predict(bn, car)
  ac<-accuracy(p_bn, car$class)
  expect_true(ac>0.9)
})

test_that("nparams not implemented", {  
  bn <- multinet_cl(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  # The number of parameters should be divided by r, where r is the number of classes, 
  # since each local model has r-1 dummy classes
  nparams(bn)
})