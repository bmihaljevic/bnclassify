context("Graph")

test_that("Make graph", {
  expect_error(make_graph(nodes = LETTERS[1], from = LETTERS[1], to=1, 
                          weights = 1))
  expect_error(make_graph(nodes = LETTERS[1], from =LETTERS[1],to=LETTERS[1],
                          weights = NULL))
  g <- make_graph(nodes = LETTERS[1:2], from =LETTERS[1],to=LETTERS[2], 
                  weights = 0.1)
  g <- make_graph(nodes = LETTERS[1], from = c(), to = c(), weights = c())
})

test_that("Complete graph", {
  g <- complete_graph(LETTERS[1:5])  
  expect_equal(length(graph::edgeNames(g)), 10)
})

test_that("Superimpose node", {
#    Nominal
  e <- list(A='B', B=NULL)
  g <- graph::graphNEL(nodes = LETTERS[1:2], edgeL = e, edgemode = "directed")
  sg <- superimpose_node(g, 'C')
  expect_equal(sort(graph::nodes(sg)), LETTERS[1:3])
  expect_equal(graph::numEdges(sg), 3L)
#    Node already in dag   
  expect_error(superimpose_node(g, 'A'), 'nodes')
})

test_that("Direct tree", {
  g <- graph::graphNEL()  
  e <- direct_tree(g)
  expect_equal(graph::edgemode(e), "directed")
  expect_equal(graph::ugraph(e), g)
  
  gr <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = 'loglik')
  af <- max_weight_forest(gr)
  d <- direct_tree(af)  
  expect_identical(graph::edgemode(d), "directed")
  d <- direct_tree(af, 'maint')
  expect_identical(graph::edgemode(d), "directed")
  d <- direct_tree(af, 'safety')
  expect_identical(graph::edgemode(d), "directed")
})

test_that("Direct forest", {
  gr <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = 'loglik') 
  af <- max_weight_forest(gr)
  f <- direct_forest(g = af)
  expect_equivalent(graph::ugraph(f), af)
})

# Function not used. 
# test_that("parents", {
#   skip_if_not_installed('gRbase')
#   set.seed(0)
#   g <- random_aug_nb_dag(class = 'A', V = letters[1:3], maxpar = 3, wgt = 0.5)
# # Nominal
#   input <- c('A', 'b', 'a')
#   output_expect <- list(character(), 'A', c('b', 'A'))
#   output <- lapply(input, parents, g)
#   expect_identical(output, output_expect)
# # Not a directed graph 
#    g <- graph::randomGraph(V = letters, M = 4, p = 0.4)
#    expect_error(parents('a', g), "directed")
# # Compare to gRbase::parents 
#   set.seed(0)
#   g <- random_aug_nb_dag(class = 'A', V = letters, maxpar = 15, wgt = 0.9)
#   expect_identical(parents('b', g), gRbase::parents('b', g))
# })

test_that("nb_dag", {
  # Nominal
  d <- nb_dag('f', letters[1:5])
  expect_equal(graph::numEdges(d), 5)
  expect_equal(graph::edgemode(d), "directed")
  # No features
  d <- nb_dag('f', NULL)
  expect_equal(graph::numEdges(d), 0)
  expect_equal(graph::numNodes(d), 1)
  expect_equal(graph::edgemode(d), "directed")
})

test_that("graph union", {
  g <- graph::graphNEL(LETTERS[1:3], edgemode = "directed")
  g <- graph::addEdge(from = "A", to = "B", g)
  
  connected <- RBGL::connectedComp(g) 
  gs <- lapply(connected, graph::subGraph, g)  
  gu <- graph_union(gs)  
  expect_identical(gu, g)
})

test_that("Max weight forest", {  
  # TODO: How many edges?
#   g <- pairwise_ode_score_contribs(class = 'Class', voting, score = 'loglik')
#   u <- max_weight_forest(g)
#   expect_equal(graph::numEdges(u), )  
  
  g <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = "loglik")
  u <- max_weight_forest(g)
  expect_equal(graph::numEdges(u), 5)  
  
# Forest
#   features <- colnames(car)[-7]
#   g <- blacklist(class='class', features=features, dataset=car, test = "mi",
#                      alpha=0.05)$filtered  
#   u <- max_weight_forest(g) # augmenting forest  
#   expect_equal(graph::numEdges(u), 4)  
})