context("bnc dag operate")

test_that("not cci nominal naive Bayes", {
  nb <- nbcar()
  s <- not_cci(nb)
  expect_equal(unlist(s, use.names = FALSE), colnames(car)[1:6])
})

test_that("not cci nominal TAN", {
  tn <- lp(chowliu('class', car), dataset = car, smooth = 1)
  s <- not_cci(tn)
  expect_true(is.list(s))
  expect_equal(length(s), 1)  
  expect_equal(length(s[[1]]), length(features(tn))) 
  
})

test_that("not cci no features", {
  nb <- nbcarclass()
  s <- not_cci(nb)
  expect_null(s)
})

test_that("include node nominal", {
  nb <- nbcarclass()
  a <- add_feature('safety', nb)
  expect_equal(features(a), 'safety')
})

test_that("include node multiple nodes", {
  nb <- nbcarclass()
  expect_error(add_feature(c('safety', 'doors'), nb), "string")
})

test_that("include node already included", {
  nb <- nbcar()
  expect_error(add_feature('safety', nb)  , "already")
})