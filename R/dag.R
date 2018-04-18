# Gets the parents of a node in the graph
# Eeach nodes' parents.
# return Named list of characters.
graphNEL_parents <- function(g) {
  nnodes <- graph::numNodes(g)
  if (nnodes == 0) return(list())
  parents <- setNames(replicate(nnodes, character()), graph::nodes(g))
  if (graph::numEdges(g) == 0) return(parents)
  # There may be no edgemode if no edges. 
  stopifnot(graph::edgemode(g) == "directed") 
  edges <- named_edge_matrix(g) # Maybe the check should be in here?
  have_parents <- tapply(unname(edges['from',]), unname(edges['to', ]),
                         identity, simplify = FALSE)
  parents[names(have_parents)] <- have_parents
  parents
} 
subgraph <- function(vars, x) {
  graph_subgraph(vars, x)
  # graph::subGraph(snodes=vars, x)
}
# TODO: keep only graph_connected_components
connected_components <- function(x) {
 graph_connected_components(x)
}
# Adds arcs from parents to node
condition_on <- function(parents, nodes, x) {
#   Replicate parents for each node 
  wparents <- rep(parents, length(nodes))
  wnodes <- rep(nodes, each = length(parents))
#   Add edges
  add_edges(wparents, wnodes, x)
}
add_edges <- function(from, to, x) {
  # check from and to are disjoint and same length
  stopifnot(is.character(from), is.character(to))
  stopifnot(are_disjoint(from, to))
  stopifnot(length(from) == length(to))
#   Consider both directions when checking the edges are not in graph already
  undirected_from <- c(from, to)
  undirected_to <- c(to, from)
  adj <- any(graph::isAdjacent(x, from = undirected_from, to = undirected_to))
  stopifnot(!adj)
  nx <- graph::addEdge(from = from, to = to, graph = x)
  stopifnot(is_dag_graph(nx))
  nx
} 
add_node <- function(node, x) {
  # graph::addNode(node, x) 
  graph_add_node(node, x)
}
remove_node <- function(node, x) {
  graph::removeNode(node, x)
}
num_arcs <- function(x) {
  graph::numEdges(x)
}