context("predict")

test_that("Maximum a posteriori", {  
  h <- nbvote()
  pred <- predict(h, voting, prob = TRUE)
  p <- map(pred)
  accu <- sum(p == voting$Class) / nrow(voting)
  expect_equal(accu, 0.9034483, tolerance = 1e-7)
})

test_that("CV a structure with no fitting args", {
  n <- nb('class', car)
  expect_error(cv(n, car, k = 5, dag = FALSE), "args")
})

test_that("CV a bnc_bn", {
  n <- lp(nb('class', car), car, 1)
  a <- cv(n, car, k = 5, dag = FALSE)
  expect_true( a > 0.5)
})

test_that("CV two bnc_bns for fitting", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  a <- cv(list(n, m), car, k = 5, dag = FALSE)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))  
})

test_that("CV two bnc_bns to repeat learning", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  # Fitting does not fail on just 2 columns, as dag is re-learned on 
  # those columns
  a <- cv(list(n, m), car[ , c(1, 7)], k = 5, dag = TRUE)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))  
  # With dag = FALSE it fails
  expect_error(cv(list(n, m), car[ , c(1, 7)], k = 5, dag = FALSE), "cols")
})

test_that("Fast structure fitting with smooth", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car[, 7, drop = FALSE]), car, smooth = 1)	
  set.seed(0)
  r <- cv(list(a, b), car, k = 10, dag = FALSE, smooth = 1)
  expect_equal(r, c(0.8541235, 0.7002352), tolerance = 1e-7)
  set.seed(0)
  s <- cv(list(a, b), car, k = 10, dag = FALSE)
  expect_equal(s, r)
})

test_that("CV a wrapper", {
  t <- tanhc('class', car, k = 2, epsilon = 0, smooth = 0.01)
  t <- lp(t, car, smooth = 0.01)
  r <- cv(t, car, k = 2, dag = TRUE)
  expect_true(is_positive(r))
})

test_that("correct cv result", {
  t <- tanhc('class', car, k = 5, epsilon = 0, smooth = 0.12)
  t <- lp(t, car, smooth = 0.01)
  set.seed(0)
  s <- cv(t, car, k = 5, dag = TRUE)
  expect_equal(s, 0.9386712, tolerance = 1e-6)
})

test_that("correct cv result with missing data", {
  nb <- nbvote()
  set.seed(0)
  s <- cv(nb, voting, k = 5, dag = TRUE)
  expect_equal(s, 0.9034483, tolerance = 1e-6)
})