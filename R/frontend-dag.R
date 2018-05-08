# TODO: merge with frontend anb

# Adds arcs from parents to node
condition_on <- function(parents, nodes, x) {
#   Replicate parents for each node 
  wparents <- rep(parents, length(nodes))
  wnodes <- rep(nodes, each = length(parents))
#   Add edges
  g <- add_edges(wparents, wnodes, x) 
  if (!skip_testing()) stopifnot(is_dag_graph(g))
  g
}