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

test_that("is supernode single node", {
  nb <- nbcar()
  expect_true(is_supernode('safety', nb))
})

test_that("is supernode two nodes not supernode", {
  nb <- nbcar()
  expect_true(!is_supernode(c('safety', 'buying'), nb))
})

test_that("is supernode two nodes supernode", {
  t <- tan_cl('class', car)
  expect_true(is_supernode(c('safety', 'buying'), t))
})

test_that("is supernode class var", {
  nb <- nbcarclass()
  expect_error(is_supernode('class', nb), "class")
})

test_that("is_semi_naive just class", {
  nb <- nbcarclass()
  expect_true(is_semi_naive(nb))
})

test_that("is_semi_naive naive Bayes", {
  nb <- nbcar()
  expect_true(is_semi_naive(nb))
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

test_that("remove feature nominal", {
  nb <- nbcar()
  rnb <- remove_feature('safety', nb)
  expect_equal(features(rnb), colnames(car)[1:5])
})

test_that("remove feature not in graph", {
  nb <- nbcarclass()
  expect_error(remove_feature('safety', nb), "not in")
})

test_that("feature orphans nominal", {
  nb <- nbcar()
  o <- feature_orphans(nb)
  expect_equal(o, features(nb))
})

test_that("feature orphans no features", {
  nb <- nbcarclass()
  o <- feature_orphans(nb)
  expect_null(o)
})

test_that("feature orphans ode", {
  nb <- nbcarp(car[, 5:7])
  nb <- add_feature_parents('lug_boot', 'safety', nb)  
  o <- feature_orphans(nb)
  expect_equal(o, 'lug_boot')
})

