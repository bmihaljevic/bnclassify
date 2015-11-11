context("cv")

test_that("learn and asses nominal", {
  n <- nbcar()
  a <- learn_and_assess(car, car, n, smooth = 1)
  p <- accuracy(predict(n, car), car$class)
  expect_equal(a, p)
})

test_that("cv fixed partition", {
  n <- nbcar()
  a <- cv_fixed_partition(n, list(car, car), list(car, car), smooth = 1)
  p <- accuracy(predict(n, car), car$class)
  expect_equal(a, p)
  
  d <- tan_cl('class', car)
  a <- cv_fixed_partition(list(n, d), list(car, car), list(car, car), smooth = 1)
  p <- accuracy(predict(n, car), car$class)
  g <- accuracy(predict(lp(d, car, smooth = 1), car), car$class)
  expect_equal(a, c(p, g))
})

test_that("make folds nominal", {
  test_make_stratified <- function() {
    f <- make_stratified_folds(car$class, 3)
    tf <- sapply(f, length)
    expect_true(max(tf) < 578, min(tf) > 574)
    tbl <- function(a) {
      table(car$class[a])
    }
    a <- lapply(f, tbl)
    diffs <- sum(abs(a[[1]] - a[[2]]), abs(a[[1]] - a[[3]]))
    expect_true(diffs <= 5)
  }
  replicate(10, test_make_stratified)
})

test_that("distribute accross folds nominal", {
  set.seed(4)
  f <- distribute_class_over_folds(1210, 3)
  expect_true(all(table(f) == c(403, 404, 403)))
  expect_error(f <- distribute_class_over_folds(1, 1))
  f <- distribute_class_over_folds(4, 4)
  expect_equal(sort(f), 1:4)
  f <- distribute_class_over_folds(1, 2)
  expect_equal(f, 1)
  f <- distribute_class_over_folds(4, 1200)
  expect_equal(f, c(733, 485, 899, 857))
})