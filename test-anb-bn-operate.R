context("bnc bn operate")

test_that("Free parameters", {    
  u <- nbcar()
  b <- nparams(x = u)
  expect_equal(b, 63)  
  
  u <- lp(nb('Class', voting), voting, smooth = 1)
  b <- nparams(u)
  expect_equal(b, 2 * 16 + 1)
})

test_that("logLik AIC BIC", {
  nb <- lp(nb('class', car), car, smooth = 0)
  ll <- logLik(nb, car)  
  expect_true(inherits(ll, "logLik"))
  expect_equal(attr(ll, "nobs"), nrow(car))
  expect_equal(attr(ll, "df"), 63)
  expect_equal(as.vector(ll), -13503.69, tolerance = 1e-6)
  aic <- AIC(nb, car)
  expect_equal(as.vector(aic), -13566.69, tolerance = 1e-6)
  bic <- BIC(nb, car)    
  expect_equal(as.vector(bic), -13738.51, tolerance = 1e-6)
})

test_that("logLik AIC BIC as bnlearn", {
  nb <- nb('class', car[, c('buying', 'class')])
  nb <- lp(nb, car, smooth = 0)  
  ll <- logLik(nb, car)  
  aic <- AIC(nb, car)
  bic <- BIC(nb, car)     
  expect_equal(as.vector(ll), -3724.18038821656273)    
  expect_equal( aic, -3739.18038821654636)  
  expect_equal( bic, -3780.09078783677614)   
  
  nb <- lp(nb('class', car), car, smooth = 0)
  ll <- logLik(nb, car)  
  aic <- AIC(nb, car)
  bic <- BIC(nb, car)     
  expect_equal(as.vector(ll), -13503.6883427047687)
  expect_equal( aic, -13566.6883427047687)
  expect_equal( bic, -13738.5120211097346)
})

test_that("manb_arc_posterior", {
  a <- nbcar()  
  expect_warning(manb_arc_posterior(a), "MANB arc posterior probabilities have not been computed for x.")
  a <- lp(a, car, smooth = 1, manb_prior = 0.1)  
  b <- manb_arc_posterior(a)
  expect_equal(names(b), features(a))
  expect_equal(as.vector(b["doors"]), 2.921702e-06, tolerance = 1e-7)
})

test_that("awnb weights", {
  a <- nbcar()  
  expect_warning(awnb_weights(a), "AWNB weights have not been computed for x.")
  a <- lp(a, car, smooth = 1, awnb_trees = 10)  
  b <- awnb_weights(a)
  expect_equal(names(b), features(a))
  expect_true(are_probs(b))
})