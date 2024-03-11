context("igraph")

test_that("Make igraph", {
  skip_if_not_installed("igraph")
  expect_error(make_graph(nodes = LETTERS[1], from = LETTERS[1], to=1, 
                          weights = 1))
  expect_error(make_graph(nodes = LETTERS[1], from =LETTERS[1],to=LETTERS[1],
                          weights = NULL))
  g <- make_graph(nodes = LETTERS[1:2], from =LETTERS[1],to=LETTERS[2], 
                  weights = 0.1)
  g <- make_graph(nodes = LETTERS[1], from = c(), to = c(), weights = c())
})

test_that("Complete graph", {
  skip_if_not_installed("igraph")
  g <- complete_graph(LETTERS[1:5])  
  g <- graph_internal2graph_NEL(g)
  expect_equal(length(igraph::E(g)), 10)
})

test_that("Superimpose node", {
  skip_if_not_installed("igraph")
#    Nominal
  g <- igraph::graph(edges = c("A", "B"), directed = TRUE)
  sg <- superimpose_node(graphNEL2_graph_internal(g), 'C')
  sg <- graph_internal2graph_NEL(sg)  
  expect_equal(sort(igraph::V(sg)$name), LETTERS[1:3])
  expect_equal(igraph::ecount(sg), 3L)
#    Node already in dag   
  expect_error(superimpose_node(graphNEL2_graph_internal(g), 'A'), 'nodes')
})


test_that("Direct forest", {
  skip_if_not_installed("igraph")
  gr <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = 'loglik') 
  af <- max_weight_forest(gr)
  f <- direct_forest(g = af)
  # check labels, weights
  igf <- graph_internal2graph_NEL(f)
  igaf <- graph_internal2graph_NEL(af)
  expect_true(igraph::graph.isomorphic(igraph::as.undirected(igf), igaf))
  expect_equal(sort(igraph::V(igf)$name), sort(igraph::V(igaf)$name))
  # weights
})

