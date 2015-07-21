context("Basic DAG")

test_that("Condition on", {
  set.seed(0)
  skip_if_not_installed('gRbase')
  g <- gRbase::random_dag(letters[1:10], maxpar = 15)
  d <- condition_on(parents = c('i', 'g', 'e'), nodes = 'a', g)
  expect_equal(graph::numEdges(d), 5)
  expect_true(is_perm(gRbase::parents('a', d), c('i', 'g', 'e')))
})

test_that("families", {
  a <- nbcar()
  l <- graphNEL_parents(a$.dag)
  expect_is(l, 'list')
  expect_equal(length(l), 7)
  expect_equal(l$buying, 'class')
  expect_equal(l$class, character())
})

