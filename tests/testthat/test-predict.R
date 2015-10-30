context("predict")

test_that("Maximum a posteriori", {  
  skip_if_not_installed('gRain')
  h <- nbvote()
  pred <- predict(h, voting, prob = TRUE)
  p <- map(pred)
  accu <- sum(p == voting$Class) / nrow(voting)
  expect_equal(accu, 0.9034483, tolerance = 1e-7)
})

test_that("CV a structure with no fitting args", {
  n <- nb('class', car)
  expect_error(cv(n, car, k = 5, dag = FALSE), "elements")
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
  skip_on_cran()
  set.seed(0)
  t <- tan_hc('class', car, k = 2, epsilon = 0, smooth = 0.01)
  t <- lp(t, car, smooth = 0.01)
  r <- cv(t, car, k = 2, dag = TRUE)
  expect_true(is_positive(r))
  expect_equal(r, 0.9386574)
})

test_that("correct cv result", {
  skip_on_cran()
  t <- tan_hc('class', car, k = 5, epsilon = 0, smooth = 0.12)
  t <- lp(t, car, smooth = 0.01)
  set.seed(0)
  s <- cv(t, car, k = 5, dag = TRUE)
  expect_equal(s, 0.9386712, tolerance = 1e-6)
})

test_that("cv with different parameter learning", {
  a <- lp(nb('class', car), car, smooth = 1)	
  set.seed(0)
  b <- lp(nb('class', car), car, smooth = 1, awnb_trees = 10)	
  r <- cv(list(a, b), car, k = 5, dag = FALSE)
  expect_true(r[1] > r[2])
})

test_that("correct cv result with missing data", {
  skip_on_cran()
  skip_if_not_installed('gRain')
  nb <- nbvote()
  set.seed(0)
  s <- cv(nb, voting, k = 5, dag = TRUE)
  expect_equal(s, 0.9034483, tolerance = 1e-6)
})

test_that("cv with just-class classifier", {
  skip_on_cran()
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car[, 'class', drop = FALSE]), car, smooth = 1)
  d <- lp(nb('class', car[, c(sample(1:6, 4), 7), drop = FALSE]), car, smooth = 1)	
  set.seed(0)
  r <- cv(list(a, b, d), car, k = 10, dag = TRUE, smooth = 1)
})