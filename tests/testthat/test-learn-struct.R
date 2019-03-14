context("Learn struct")

test_that("nb  Nominal", {
  n <- nb('class', car)
  expect_identical('class', class_var(n))
  expect_equal(graph_num_arcs(dag(n)), 6) 
})

test_that("nb   Class not in dataset", {
  expect_error(nb('Class', car), 'disjoint')
})

test_that("nb No features ", {
  n <- nb('class', car[, 7, drop = FALSE])
  expect_equal(graph_num_arcs(dag(n)), 0)
  # Not numeric dataset
})

test_that("fssj nominal", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  f <- fssj('class', dataset = car, k = 10, epsilon = 0.01)
  expect_equal(features(f), character()) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  f <- fssj('class', dataset = car, k = 10, epsilon = 0)
  expect_true(is_perm(features(f), colnames(car)[-7]))
})

test_that("bsej nominal", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0.01)
  expect_equal(features(f), colnames(car)[-7])
  expect_equal(narcs(f), 6 + 1 + 2 + 3)
  expect_equal(length(f$.greedy_scores_log), 4)
  expect_equal(f$.greedy_scores_log, sort(f$.greedy_scores_log)) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  f <- bsej('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(features(f), colnames(car)[-7])
  expect_equal(narcs(f), 13)
  expect_equal(length(f$.greedy_scores_log), 5)
  expect_equal(f$.greedy_scores_log, sort(f$.greedy_scores_log))
})

test_that("tan_hc nominal", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  t <- tan_hc('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(length(features(t)), 6)
  expect_equal(narcs(t), 11)
})

test_that("tanhc sp nominal", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  t <- tan_hcsp('class', dataset = car, k = 10, epsilon = 0)
  expect_equal(length(features(t)), 6)
  expect_equal(narcs(t), 11)
  nfams <- sapply(families(t), length)
  expect_true(max(nfams) < 5) 
}) 

test_that("kdb nominal", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  t <- kdb('class', dataset = car, kdb = 1, k = 10, epsilon = 0) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  to <- tan_hc('class', dataset = car, k = 10, epsilon = 0)
  expect_false(isTRUE(all.equal(t, to)))
  t$.call_struct <- NULL
  to$.call_struct <- NULL
  expect_true(isTRUE(all.equal(t, to)))
})  