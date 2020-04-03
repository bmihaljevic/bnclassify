context("inference")   

test_that("plot", {
  bn <- multinet_tan(class = 'class', dataset = car)
  # Should be a message instead of an error
  plot(bn) 
}) 

test_that("Predict", { 
  bn <- multinet_tan(class = 'class', dataset = car)
  bn <- lp(bn, car, smooth = 0.1)
  a <- compute_cp(x = bn, car)
})  

test_that("bnc function", { 
  nb <- bnc()
  plot(nb)
})