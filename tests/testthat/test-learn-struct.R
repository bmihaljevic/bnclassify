context("Learn struct")

test_that("nb", {
  # Nominal
  n <- nb('class', car)
  expect_identical('class', bnc_class(n))
  expect_equal(graph::numEdges(to_graphNEL(n)), 6) 
  # Class not in dataset
  expect_error(nb('Class', car), 'disjoint')
  # No features 
  n <- nb('class', car[, 7, drop = FALSE])
  expect_equal(graph::numEdges(to_graphNEL(n)), 0)
  # Not numeric dataset
})