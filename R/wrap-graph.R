# TODO: requiere message for graph package

# TODO: will use this function for conversion to graphNEL
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
      # weights <- unlist(weights)
      graph_internal(nodes, edges, weights, edgemode) 
  }
  else stop()
}  
graph_internal2graph_NEL <- function(x) {  
  
  if (!requireNamespace("graph", quietly = TRUE)) {
    stop("Package graph required for this functionality.")
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
      
      # Assuming x$edgemode is a character indicating either "directed" or "undirected"
      edgemode <- ifelse(x$edgemode == "directed", TRUE, FALSE)
      
      # Build the igraph graph
      # , weights = weights
      igraph::graph_from_edgelist(edges, directed = edgemode)
}  
# 
#' Returns an edge matrix with node names (instead of node indices).
#' 
#' TODO Not sure this function is used.
#' 
#' @param g A graphNEL.
#' @return A character matrix. 
#' @keywords internal
named_edge_matrix <- function(g) { 
  if (requireNamespace("graph", quietly = TRUE)) {
      u <- graph::edgeMatrix(g)
      u[] <- graph::nodes(g)[as.vector(u)]
      if (length(u) == 0) mode(u) <- 'character'
      stopifnot(is.character(u))
      u 
  }
  else stop()
}