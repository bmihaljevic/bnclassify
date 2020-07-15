context("graph internal")
  
test_that("empty graph", { 
   a <- graph_internal()
   expect_equal(length(a$nodes), 0)
   expect_equal(length(graph_nodes(a)), 0)
   expect_is(a$edges, "matrix")
   expect_true(mode(a$edges) == "character")
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
  expect_equal(length(g), 1)
  
  g <- graph_internal()
  g <- graph_connected_components(g)
  expect_null(g)  
})

test_that("graph union", { 
  edges <- graph_from_to_to_edges('A', 'B')
  g <- graph_internal(LETTERS[1:3], edges = edges, edgemode = "directed" ); 
  connected <- graph_connected_components(g) 
  gs <- lapply(connected, subgraph, g)  
  gu <- graph_union(gs)  
  # Currently not identical because I am missing to set some attributes in graph_internal2graph_NEL 
  # expect_identical(gu, g)
  expect_identical(graph_nodes(gu), graph_nodes(g))
  expect_equal(gu$edges, g$edges)
})

test_that("Direct tree", {
  g <- graph_internal()
  e <- direct_tree(g)
  expect_equal(e$edgemode, "directed")
  # expect_equal(graph::ugraph(graph_internal2graph_NEL(e)), graph_internal2graph_NEL(g))
  
  gr <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = 'loglik')
  af <- max_weight_forest(gr)
  d <- direct_tree(af)   
  expect_equal(d$edgemode, "directed")
  d <- direct_tree(af, 'maint')
  expect_equal(d$edgemode, "directed")
  d <- direct_tree(af, 'safety')
  expect_equal(d$edgemode, "directed")
})

test_that("node parents no parents", {  
  g <- graph_internal()
  expect_error(graph_node_parents('A', g))
  
  p <- graph_node_parents('A', graph_internal('A')) 
  expect_equal(p, character())
})
