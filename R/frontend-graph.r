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
# TODO: keep only graph_connected_components
connected_components <- function(x) {
 graph_connected_components(x)
}

add_edges <- function(from, to, x) {
#   # check from and to are disjoint and same length
#   stopifnot(is.character(from), is.character(to))
#   stopifnot(are_disjoint(from, to))
#   stopifnot(length(from) == length(to))
# #   Consider both directions when checking the edges are not in graph already
#   undirected_from <- c(from, to)
#   undirected_to <- c(to, from)
#   adj <- any(graph::isAdjacent(x, from = undirected_from, to = undirected_to))
#   stopifnot(!adj)
#   nx <- graph::addEdge(from = from, to = to, graph = x)
#   nx 
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