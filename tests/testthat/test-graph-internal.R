context("graph internal")

test_that("graphNEL 2 graph internal", {
  skip_if_not_installed('graph')
  x <- nbcar() 
  g <- graphNEL2_graph_internal(x$.dag)
  test_make(g$nodes, g$edges)
  # not checking anything. just to see it works. 
})

test_that("connected components", { 
  x <- nbcar()  
  g <- graph_connected_components(x$.dag)
  g 
})