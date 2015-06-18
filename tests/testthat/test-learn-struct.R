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
  expect_equal(features(f), character())
  set.seed(0)
  f <- fssj('class', dataset = car, k = 10, epsilon = 0)
  expect_true(is_perm(features(f), colnames(car)[-7]))
})

test_that("bsej nominal", {
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0.01)
  expect_equal(features(f), colnames(car)[-7])
  expect_equal(narcs(f), 6 + 1 + 2 + 3)
  expect_equal(length(f$.greedy_scores_log), 4)
  expect_equal(f$.greedy_scores_log, sort(f$.greedy_scores_log))
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(features(f), colnames(car)[-7])
  expect_equal(narcs(f), 13)
  expect_equal(length(f$.greedy_scores_log), 5)
  expect_equal(f$.greedy_scores_log, sort(f$.greedy_scores_log))
})

test_that("tanhc nominal", {
  set.seed(0)
  t <- tanhc('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(length(features(t)), 6)
  expect_equal(narcs(t), 9)
})

test_that("tanhc sp nominal", {
  set.seed(0)
  t <- tanhc_sp('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(length(features(t)), 6)
  expect_equal(narcs(t), 10)
})