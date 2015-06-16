context("Learn struct")

test_that("nb  Nominal", {
  n <- nb('class', car)
  expect_identical('class', class_var(n))
  expect_equal(graph::numEdges(to_graphNEL(n)), 6) 
})

test_that("nb   Class not in dataset", {
  expect_error(nb('Class', car), 'disjoint')
})

test_that("nb No features ", {
  n <- nb('class', car[, 7, drop = FALSE])
  expect_equal(graph::numEdges(to_graphNEL(n)), 0)
  # Not numeric dataset
})

test_that("fssj nominal", {
  set.seed(0)
  f <- fssj('class', dataset = car, k = 10, epsilon = 0.01)
  expect_equal(features(f$model), )
  set.seed(0)
  f <- fssj('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(features(f$model), )
})

test_that("bsej nominal", {
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0.01)
  expect_equal(features(f$model), colnames(car)[-7])
  expect_equal(narcs(f$model), 6 + 1 + 2 + 3)
  expect_equal(length(f$log), 4)
  expect_equal(f$log, sort(f$log))
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(features(f$model), colnames(car)[-7])
  expect_equal(narcs(f$model), 13)
  expect_equal(length(f$log), 5)
  expect_equal(f$log, sort(f$log))
})