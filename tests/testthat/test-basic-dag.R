context("Basic DAG")

test_that("Condition on", {
  set.seed(0)
  g <- gRbase::random_dag(letters[1:10], maxpar = 15)
  d <- condition_on(node = 'a', parents = c('i', 'g', 'e'), g)
  expect_equal(graph::numEdges(d), 5)
  expect_true(is_perm(gRbase::parents('a', d), c('i', 'g', 'e')))
})
