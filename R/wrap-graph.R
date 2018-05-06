# TODO: requiere message for graph package

# TODO: will use this function for conversion to graphNEL
graphNEL2_graph_internal <- function(x) { 
  stopifnot(inherits(x, "graphNEL"))
  if (requireNamespace("graph", quietly = TRUE)) {
      nodes <- graph::nodes(x)
      edges <- t(named_edge_matrix(x)) 
      weights <- NULL 
      tryCatch({
      # Will fail if it does not have the attribute.
       weights <- graph::edgeData(self = x, from = edges[, 1], to = edges[, 2], 
                                 attr = "weight")  
      }, error = function(e) {}) 
      weights <- unlist(weights)
      edgemode <- graph::edgemode(x)
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
      graph::ftM2graphNEL(ft = edges, W = weights, V = x$nodes, edgemode = x$edgemode)  
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