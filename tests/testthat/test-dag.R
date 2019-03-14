context("Basic DAG")

test_that("Condition on", {
  skip_if_not_installed('gRbase') 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  g <- gRbase::random_dag(letters[1:10], maxpar = 15)
  g <- graphNEL2_graph_internal(g)
  d <- condition_on(parents = c('i', 'g', 'e'), nodes = 'a', g)
  expect_equal(graph_num_arcs(d), 5)
  d <- graph_internal2graph_NEL(d)
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

