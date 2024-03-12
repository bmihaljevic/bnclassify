graphNEL2_graph_internal <- function(x) {
  stopifnot(inherits(x, "igraph"))
  nodes <- igraph::V(x)$name
  edges <- igraph::as_edgelist(x)  # Assuming 'x' is an igraph object
  colnames(edges) <- c("from", "to")
  # edges <- t(named_edge_matrix(x)) 
  weights <- NULL
  tryCatch({
    # Will fail if it does not have the attribute.
    weights <- igraph::E(x)$weight
  }, error = function(e) {})
  edgemode <- ifelse(igraph::is_directed(x), "directed", "undirected")
  weights <- igraph::E(x)$weight
  graph_internal(nodes, edges, weights, edgemode) 
}  
graph_internal2graph_NEL <- function(x) {
    stopifnot(inherits( x, "bnc_graph_internal")) 
    edges <- x$edges 
    edgemode <- ifelse(x$edgemode == "directed", TRUE, FALSE)
    graph <- igraph::graph_from_edgelist(edges, directed = edgemode)
    weights <- x$weight
    if (length(weights ) > 0) {
      stopifnot(length(weights ) == length(igraph::E(graph)))
      igraph::set_edge_attr(graph, "weight", value = weights)
    }
    # TODO: handle undirected. If directed, then build directed graph in BH.
    graph 
}  