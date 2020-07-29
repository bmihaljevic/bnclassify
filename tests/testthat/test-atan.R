context("ATAN")

test_that("Predict", { 
  bn <- multinet_atan(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  p_bn <- predict(bn, car)
  ac<-accuracy(p_bn, car$class)
  expect_true(ac>0.9)
})  

test_that("Predict", { 
  bn <- multinet_atan(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  a <- compute_cp(x = bn, car)
})  


test_that("check number of models and class", { 
  bn <- multinet_atan(class = 'class', dataset = car)
  expect_equal(class(bn), c("bnc_ensemble", "bnc_base"))
  expect_true(length(bn$.models) == length(names(car))-1)
})




