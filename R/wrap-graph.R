# TODO: requiere message for igraph package
graphNEL2_graph_internal <- function(x) { 
  stopifnot(inherits(x, "igraph"))
  if (requireNamespace("igraph", quietly = TRUE)) {
      nodes <- igraph::V(x)$name
      edges <- igraph::get.edgelist(x)  # Assuming 'x' is an igraph object
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
  else stop()
}  
graph_internal2graph_NEL <- function(x) {  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package igraph required for this functionality.")
  }
      stopifnot(inherits( x, "bnc_graph_internal")) 
      edges <- x$edges 
      weights <- x$weight
      if (length(weights ) == 0) {
        # ftM2graphNEL fails with a zero length W vector
        weights <- NULL 
      }
      # TODO: handle undirected. If directed, then build directed graph in BH.
      # graph::ftM2graphNEL(ft = edges, W = weights, V = x$nodes, edgemode = x$edgemode)  
      edgemode <- ifelse(x$edgemode == "directed", TRUE, FALSE)
      
      # Build the igraph graph
      graph <- igraph::graph_from_edgelist(edges, directed = edgemode)
      # igraph::E(graph)$weight <- weights
      igraph::set_edge_attr(graph, "weight", value = weights)
      graph 
}  