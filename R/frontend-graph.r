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
  graph_complete_undirected(nodes) 
} 
make_graph <- function(nodes, from, to, weights) {      
  edges <- graph_from_to_to_edges(from, to) 
  # TODO: change name to make_ugraph
  g <- graph_internal(nodes, edges, weights, "undirected") 
  graph_internal2graph_NEL(g)
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