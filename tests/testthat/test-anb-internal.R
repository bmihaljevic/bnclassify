context("anb internal") 

test_that("nb_dag", {
  # Nominal
  d <- nb_dag('f', letters[1:5])
  expect_equal(graph_num_arcs(d), 5)
  expect_equal(d$edgemode, "directed")
  # No features
  d <- nb_dag('f', NULL)
  expect_equal(graph_num_arcs(d), 0)
  expect_equal(graph_num_nodes(d), 1) 
  expect_equal(d$edgemode, "directed")
})
