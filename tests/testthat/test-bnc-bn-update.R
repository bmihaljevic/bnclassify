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
  diff <- sum(abs(bnc_params(b)[[2]]  - bnc_params(dgcar)[[2]]))
	expect_equal(diff, 2.393358, tolerance = 1e-7)
})

test_that("bnc update bnc_bn with struct learning", {
  dgcar <- lp(nb('class', car), car, smooth = 1)	
  ua <- bnc_get_update_args(dgcar, dag = TRUE)
  b <- bnc_update(ua, car[1:5, 6:7])
  expect_identical(bnc_features(b), "safety")
  diff <- sum(abs(bnc_params(b)[['safety']]  - bnc_params(dgcar)[['safety']]))
  expect_equal(diff, 2.776258, tolerance = 1e-6)
})

test_that("multi learn nominal", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  d <- multi_learn(list(a, b), smooth = 1, car)
  expect_equal(length(d), 2)
  # lapply(d, cpt_vars_values). Should really clean this up.
  #   TODO: do I need to make sure that the name of the CPT in the list corresponds to the feature? I will probably remove that assumption. Or maybe not. 
})

test_that("multi learn predict nominal", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  multi_learn_predict(list(a, b), smooth = 1, car[, -7])
})