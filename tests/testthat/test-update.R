context("Update")

# ==============================================================================
# To delete

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

# ==============================================================================

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
  t <- lp(tan_bnc('class', car), car, smooth = 0.02)
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
  a <- lp(tan_bnc('class', car), car, smooth = 1e-20)
  b <- lp(nb('class', car), car, smooth = 1e-20)
  c <- multi_update(list(a, b), car[1:5, ], dag = TRUE)
  expect_equal(narcs(c[[1]]), narcs(a))
  expect_true(!isTRUE(all.equal(families(c[[1]]), families(a))))
  expect_equal(families(c[[2]]), families(b))
})

test_that("Update dag", {
  t <- tan_bnc('class', car)
  d <- update_dag(t, car[1, ])  
  expect_identical(narcs(d), narcs(t))
  expect_true(!identical(families(d), families(t)))
})

#  TODO: delete
# test_that("multi learn predict nominal class values", {
#   a <- nbcar()  
#   b <- nbcarp(car[, 4:7])
#   d <- multi_learn_predict(list(a, b), train = car, test = car, smooth = 1,
#                            prob = FALSE)
#   e <- predict(a, car)
#   expect_equal(d[[1]], e)
#   f <- predict(b, car)
#   expect_equal(d[[2]], f)
# })
# 
# test_that("multi learn predict single row test set", {
#   a <- nbcar()  
#   b <- nbcarp(car[, 4:7])
#   t <- car[1, , drop = FALSE]
#   d <- multi_learn_predict(list(a, b), train = car, test = t, smooth = 1,
#                            prob = FALSE)
#   e <- predict(a, t)
#   expect_equal(d[[1]], e)
#   f <- predict(b, t)
#   expect_equal(d[[2]], f)
# })
# 
# test_that("multi learn predict probs", {
#   a <- nbcar()  
#   b <- nbcarp(car[, 4:7])
#   d <- multi_learn_predict(list(a, b), train = car, test = car, 
#                            smooth = 1, prob = TRUE)
#   e <- compute_augnb_luccpx(a, car)
#   expect_equal(d[[1]], e)
#   f <- compute_augnb_luccpx(b, car)
#   expect_equal(d[[2]], f)
# })