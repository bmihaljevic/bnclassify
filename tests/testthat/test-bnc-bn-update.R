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

test_that("multi learn predict nominal", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  debugonce(compute_augnb_lucp_multi)
  d <- multi_learn_predict(list(a, b), car, smooth = 1)
  expect_equal(length(d), 2)
})
