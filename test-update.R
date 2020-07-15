context("Update")

test_that("bnc update bnc_dag", {
  dgcar <- nb('class', car)  
  # Currently, parameter fitting is required for updating; dag learning is not
  expect_error(bnc_get_update_args(dgcar, dag = FALSE), "lp_fargs")
})

test_that("bnc update bnc_bn", {
	dgcar <- lp(nb('class', car), car, smooth = 1)	
	ua <- bnc_get_update_args(dgcar, dag = FALSE)
	b <- bnc_update(ua, car[1:5, ])  
  diff <- sum(abs(params(b)[[2]]  - params(dgcar)[[2]]))
	expect_equal(diff, 2.393358, tolerance = 1e-7)
})

test_that("bnc update bnc_bn with struct learning", {
  dgcar <- lp(nb('class', car), car, smooth = 1)	
  ua <- bnc_get_update_args(dgcar, dag = TRUE)
  b <- bnc_update(ua, car[1:5, 6:7])
  expect_identical(features(b), "safety")
  diff <- sum(abs(params(b)[['safety']]  - params(dgcar)[['safety']]))
  expect_equal(diff, 2.776258, tolerance = 1e-6)
})

test_that("update nominal", {
  a <- lp(nb('class', car), car, smooth = 1e10)	  
  b <- update(a, car, dag = FALSE)
  identical_non_call(a, b) 
})

test_that("update data subset", {
  a <- lp(nb('class', car), car, smooth = 1)
  b <- update(a, car[1:5, ], dag = FALSE)
  diff <- sum(abs(params(a)[[2]]  - params(b)[[2]]))
  expect_equal(diff, 2.393358, tolerance = 1e-7)
})

test_that("update with dag", {
  t <- lp(tan_cl('class', car), car, smooth = 0.02)
  b <- update(t, car[1:5, ], dag = TRUE)
  expect_equal(narcs(b), narcs(t))
  expect_true(!isTRUE(all.equal(families(b), families(t))))
})

test_that("update with dag 2", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- update(a, car[1:5, 6:7], dag = TRUE)
  expect_identical(features(b), "safety")
  diff <- sum(abs(params(b)[['safety']]  - params(a)[['safety']]))
  expect_equal(diff, 2.776258, tolerance = 1e-6)
})

test_that("Update dag", {
  t <- tan_cl('class', car)
  d <- update_dag(t, car[1, ])  
  expect_identical(narcs(d), narcs(t))
  expect_true(!identical(families(d), families(t)))
})

test_that("Update with awnb param learning", { 
  # gRain implementation change
  # skip_on_cran()
  # skip_if_not_installed('gRain')
  # a <- nb('Class', voting) 
  # suppressWarnings(RNGversion("3.5.0"))
  # set.seed(0)
  # b <- lp(a, voting, smooth = 1, awnb_trees = 1, awnb_bootstrap = 0.5)
  # c <- lp(a, voting, smooth = 1, awnb_trees = 45, awnb_bootstrap = 1)
  # d <- lp(b, voting, smooth = 1)
  # r <- cv(list(b, c, d), voting, k = 2, dag = FALSE)
  # # All three values are different
  # expect_equal(r, c(0.9517397, 0.9494462, 0.8988606), tolerance = 1e-6) 
  # gRain implementation change
})

test_that("Multi-update bnc_dag", {
  a <- nb('class', car)
  b <- lp(a, car, smooth = 1)
  expect_error(cv(list(a, b), car, k = 10, dag = FALSE), "must inherit")
})

test_that("Update with non-name function", {
  dgcar <- lp(nb('class', car), car, smooth = 1)	
  e <- lapply(list(dgcar), lp, car, smooth = 1)
  ua <- bnc_get_update_args(e[[1]], dag = FALSE)
  b <- bnc_update(ua, car[1:5, ])  
  diff <- sum(abs(params(b)[[2]]  - params(dgcar)[[2]]))
  expect_equal(diff, 2.393358, tolerance = 1e-7)
})