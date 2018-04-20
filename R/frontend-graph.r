 # TODO use only graph internal and remove these functions.

# Gets the parents of a node in the graph
# Eeach nodes' parents.
# return Named list of characters.
graphNEL_parents <- function(g) {  
  graph_parents(g)
} 
subgraph <- function(vars, x) {
  graph_subgraph(vars, x)
}
connected_components <- function(x) {
 graph_connected_components(x)
}

add_edges <- function(from, to, x) { 
  graph_add_edges(from, to, x)
} 
add_node <- function(node, x) {
  graph_add_node(node, x)
}
remove_node <- function(node, x) {
  graph_remove_node(node, x)
}
num_arcs <- function(x) {
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
