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