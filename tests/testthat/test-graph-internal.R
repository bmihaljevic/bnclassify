context("graph internal")

test_that("graphNEL 2 graph internal", {
  skip_if_not_installed('graph')
  x <- nbcar() 
  g <- graphNEL2_graph_internal(x$.dag)
  # not checking anything. just to see it works. 
}) 

test_that("empty graph", { 
   a <- graph_internal()
   expect_null(a$nodes)
   expect_null(graph_nodes(a))
   expect_is(a$edges, "matrix")
   expect_true(mode(a$edges) == "numeric")
})
  
test_that("get adjacent", {  
  e <- graph_from_to_to_edges('a', 'b') 
  a <- graph_internal(letters[1:5], e)
  expect_equal(graph_get_adjacent("a", a), 'b')
  expect_equal(graph_get_adjacent("b", a), 'a')
  expect_equal(graph_get_adjacent("c", a), character())
  expect_error(graph_get_adjacent("z", a))
})

test_that("connected components", { 
  x <- nbcar()  
  g <- graph_connected_components(x$.dag)
  g 
})