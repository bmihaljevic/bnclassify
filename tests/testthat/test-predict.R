context("predict")

test_that("Maximum a posteriori", {  
  h <- nbvote()
  pred <- predict(h, voting, prob=T)
  p <- map(pred)
  accu <- sum(p == voting$Class) / nrow(voting)
  expect_equal(accu, 0.9034483, tolerance = 1e-7)
})

test_that("CV a structure with no fitting args", {
  n <- nb('class', car)
  expect_error(bnc_cv(n, car, k = 5), "lp")
})

test_that("CV a bnc_bn", {
  n <- lp(nb('class', car), car, 1)
  a <- bnc_cv(n, car, k = 5)
  expect_true( a > 0.5)
})

test_that("CV two bnc_bns for fitting", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  a <- bnc_cv(list(n, m), car, k = 5)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))  
})

test_that("CV two bnc_bns to repeat learning", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  # Fitting does not fail on just 2 columns, as dag is re-learned on those columns
  a <- bnc_cv(list(n, m), car[ , c(1, 7)], k = 5, dag = TRUE)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))  
  # With dag = FALSE it fails
  expect_error(bnc_cv(list(n, m), car[ , c(1, 7)], k = 5), "cols")
})

test_that("CV structures for fitting", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car[, 7, drop = FALSE]), car, smooth = 1)	
  set.seed(0)
  r <- dag_cv(list(a, b), 'class', car, smooth = 1, k = 10)
  expect_equal(length(r), 2)
  expect_true(all(r > 0.5))
  a <- lp(nb('class', car), car, smooth = 1)	
  set.seed(0)
  d <- bnc_cv(list(a, b), car, k = 10, dag = FALSE)
  expect_equal(r, d)
})