 # TODO use only graph internal and remove these functions.

# Gets the parents of a node in the graph
# Eeach nodes' parents.
# return Named list of characters.
graphNEL_parents <- function(g) {  
  graph_parents(g)
} 
subgraph <- function(vars, x) {
  # graph_subgraph(vars, x)
  graph::subGraph(snodes=vars, x)
}
connected_components <- function(x) {
 graph_connected_components(x)
}

add_edges <- function(from, to, x) { 
  graph_add_edges(from, to, x)
} 
add_node <- function(node, x) {
  # graph::addNode(node, x) 
  graph_add_node(node, x)
}
remove_node <- function(node, x) {
  # graph::removeNode(node, x) 
  graph_remove_node(node, x)
}
num_arcs <- function(x) {
  # graph::numEdges(x)
  graph_num_arcs(x)
}

#' Returns a complete unweighted graph with the given nodes.
#' 
#' @param nodes A character vector.
#' @return a \code{graphNEL} object.
#' @keywords internal
complete_graph <- function(nodes) {   
  g <- graph::graphNEL(nodes)
  graph::complement(g)
} 
make_graph <- function(nodes, from, to, weights) {  
# Check nodes is character 
  stopifnot(is.character(nodes))
#  Make graph from nodes 
  g <- graph::graphNEL(nodes)  
#  Check lengths of from, to, weights all the same
  stopifnot(length(from) == length(to))
  stopifnot(length(to) == length(weights))
# If no edges to add, return graph 
  if (length(from) == 0)  { return (g) }    
#  Check from, to character and weights numeric
  stopifnot(is.character(from))
  stopifnot(is.character(to))
  stopifnot(is.numeric(weights))
#  Add edges from to with weights   
  g <- graph::addEdge(from = from, to = to, graph = g, weights = weights)
#  Check sum of weights in g is sum of weights
  w <- unlist(graph::edgeWeights(g))
  weights_correct <- all.equal(sum(w), sum(weights) * 2, tolerance = 1E-10)
  stopifnot(weights_correct)
# Return graph   
  g
}  
#' Returns an edge matrix with node names (instead of node indices).
#' 
#' @param g A \code{\link{graphNEL}}
#' @return A character matrix. 
#' @keywords internal
named_edge_matrix <- function(g) {
  u <- graph::edgeMatrix(g)
  u[] <- graph::nodes(g)[as.vector(u)]
  if (length(u) == 0) mode(u) <- 'character'
  stopifnot(is.character(u))
  u
}