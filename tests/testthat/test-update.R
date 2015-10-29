context("Update")

test_that("bnc update bnc_dag", {
  dgcar <- nb('class', car)  
  # Currently, parameter fitting is required for updating; dag learning is not
  expect_error(bnc_get_update_args(dgcar, dag = FALSE), "lp")
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

identical_non_call <- function(x, y) {
  x$.call_struct <- y$.call_struct <- NULL
  x$.call_bn <- y$.call_bn <- NULL 
  expect_identical(x, y)
}

test_that("Multi update single", {
  a <- lp(nb('class', car), car, smooth = 1e10)	  
  b <- multi_update(a, car, dag = FALSE)
  identical_non_call(a, b[[1]]) 
})

test_that("Multi update data subset", {
  a <- lp(nb('class', car), car, smooth = 1)
  b <- multi_update(a, car[1:5, ], dag = FALSE)
  diff <- sum(abs(params(a)[[2]]  - params(b[[1]])[[2]]))
  expect_equal(diff, 2.393358, tolerance = 1e-7)
})

test_that("Multi update with dag", {
  t <- lp(tan_cl('class', car), car, smooth = 0.02)
  b <- multi_update(t, car[1:5, ], dag = TRUE)
  expect_equal(narcs(b[[1]]), narcs(t))
  expect_true(!isTRUE(all.equal(families(b[[1]]), families(t))))
})

test_that("Multi update with dag 2", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- multi_update(a, car[1:5, 6:7], dag = TRUE)
  expect_identical(features(b[[1]]), "safety")
  diff <- sum(abs(params(b[[1]])[['safety']]  - params(a)[['safety']]))
  expect_equal(diff, 2.776258, tolerance = 1e-6)
})

test_that("Multi update with dag with different lp args", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car), car, smooth = 2)	
  c <- multi_update(list(a, b), car, dag = TRUE)
  identical_non_call(c[[1]], a)
  identical_non_call(c[[2]], b)
})

test_that("Multi update with dag two", {
  a <- lp(tan_cl('class', car), car, smooth = 1e-20)
  b <- lp(nb('class', car), car, smooth = 1e-20)
  c <- multi_update(list(a, b), car[1:5, ], dag = TRUE)
  expect_equal(narcs(c[[1]]), narcs(a))
  expect_true(!isTRUE(all.equal(families(c[[1]]), families(a))))
  expect_equal(families(c[[2]]), families(b))
})

test_that("Update dag", {
  t <- tan_cl('class', car)
  d <- update_dag(t, car[1, ])  
  expect_identical(narcs(d), narcs(t))
  expect_true(!identical(families(d), families(t)))
})

test_that("Update with awnb param learning", {
  skip_on_cran()
  skip_if_not_installed('gRain')
  a <- nb('Class', voting)
  set.seed(0)
  b <- lpawnb(a, voting, smooth = 1, trees = 1, bootstrap_size = 0.5)
  c <- lpawnb(a, voting, smooth = 1, trees = 45, bootstrap_size = 1)
  d <- lp(b, voting, smooth = 1)
  r <- cv(list(b, c, d), voting, k = 2, dag = FALSE)
  # All three values are different
  expect_equal(r, c(0.9563269, 0.9494356, 0.9011225), tolerance = 1e-6)
})

test_that("Multi-update bnc_dag", {
  a <- nb('class', car)
  b <- lp(a, car, smooth = 1)
  expect_error(cv(list(a, b), car, k = 10, dag = FALSE), "must inherit")
})