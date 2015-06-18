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

test_that("Update dag", {
  t <- tan_bnc('class', car)
  d <- update_dag(t, car[1, ])  
  expect_identical(narcs(d), narcs(t))
  expect_true(!identical(families(d), families(t)))
})

test_that("bnc update bnc_bn with struct learning", {
  dgcar <- lp(nb('class', car), car, smooth = 1)	
  ua <- bnc_get_update_args(dgcar, dag = TRUE)
  b <- bnc_update(ua, car[1:5, 6:7])
  expect_identical(features(b), "safety")
  diff <- sum(abs(params(b)[['safety']]  - params(dgcar)[['safety']]))
  expect_equal(diff, 2.776258, tolerance = 1e-6)
})

test_that("multi learn predict probs", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  d <- multi_learn_predict(list(a, b), train = car, test = car, 
                           smooth = 1, prob = TRUE)
  e <- compute_augnb_luccpx(a, car)
  expect_equal(d[[1]], e)
  f <- compute_augnb_luccpx(b, car)
  expect_equal(d[[2]], f)
})

test_that("multi learn predict nominal class values", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  d <- multi_learn_predict(list(a, b), train = car, test = car, smooth = 1,
                           prob = FALSE)
  e <- predict(a, car)
  expect_equal(d[[1]], e)
  f <- predict(b, car)
  expect_equal(d[[2]], f)
})

test_that("multi learn predict single row test set", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  t <- car[1, , drop = FALSE]
  d <- multi_learn_predict(list(a, b), train = car, test = t, smooth = 1,
                           prob = FALSE)
  e <- predict(a, t)
  expect_equal(d[[1]], e)
  f <- predict(b, t)
  expect_equal(d[[2]], f)
})