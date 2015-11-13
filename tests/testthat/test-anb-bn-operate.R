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
  skip_if_not_installed('bnlearn')
  bnlearn_mle_nb <- function(data, training) {
    features <- setdiff(colnames(data), training)
    nb.bn <- bnlearn::naive.bayes(x = car, training = training, 
                                  explanatory = features)
    bnlearn::bn.fit(x = nb.bn, data = data, method = "mle")    
  }  
  
  nb <- nb('class', car[, c('buying', 'class')])
  nb <- lp(nb, car, smooth = 0)  
  ll <- logLik(nb, car)  
  aic <- AIC(nb, car)
  bic <- BIC(nb, car)    
  nb.bn <- bnlearn_mle_nb(car[,c('buying','class')], 'class')
  ll.bn <- bnlearn:::logLik.bn.fit(object = nb.bn, 
                                   data = car[,c('buying','class')])
  aic.bn <- bnlearn:::AIC.bn.fit(object = nb.bn, 
                                 data = car[,c('buying','class')])
  bic.bn <- bnlearn:::BIC.bn.fit(object = nb.bn, 
                                 data = car[,c('buying','class')])
  expect_equal(as.vector(ll), ll.bn)  
  expect_equal( aic, aic.bn)  
  expect_equal( bic, bic.bn)
  
  nb <- lp(nb('class', car), car, smooth = 0)
  ll <- logLik(nb, car)  
  aic <- AIC(nb, car)
  bic <- BIC(nb, car)    
  nb.bn <- bnlearn_mle_nb(car, 'class')
  ll.bn <- bnlearn:::logLik.bn.fit(object = nb.bn, data = car)
  aic.bn <- bnlearn:::AIC.bn.fit(object = nb.bn, data = car)
  bic.bn <- bnlearn:::BIC.bn.fit(object = nb.bn, data = car)
  expect_equal(as.vector(ll), ll.bn)
  expect_equal( aic, aic.bn)
  expect_equal( bic, bic.bn)
})

test_that("arc_posterior", {
  a <- nbcar()  
  expect_warning(arc_posterior(a), "MANB arc posterior probabilities have not been computed for x.")
  a <- lp(a, car, smooth = 1, manb_prior = 0.1)  
  b <- arc_posterior(a)
  expect_equal(names(b), features(a))
  expect_equal(as.vector(b[3]), 2.921702e-06, tolerance = 1e-7)
})