# all functions with begin wit graph_ 
# Nodes + edge list. 

# TODO: implement the interfac of dag.r.  
# TODO: also cover anb-families. Most of it anyway. Because no need for ANB specifics here. That is, the class variable is indistinct here.
# However, soma of anb-families will probably go away.

# TODO: the matrix list should contain integers, not strings, to avoid back and forth with boost
#     a print function could translate the integers to strings
# 
# nodes()
# add ..
# I only need boost for algorithms and things I do not implement
# is_dag() : here call  the acyclic check   

# /**
#  * Basic adjacency list object api
#  * Data:
#  *  mapping from names to ids.
#  *  Matrix of edges between
#  * adj_list {
#  *    string_vec nodes : all the string nodes I need.
#  *    edge matrix: uses ids as entries
#  *   public
#  *    int vec ids():
#  *    edgeMatrix(names=FALSE): uses ids as entries
#  *    edgeMatrix(names=FALSE): uses names as entries
#  *    print(): print nicely
#  * }
#  */   
valid_weights <- function(weights, edges) {
  is.null(weights) || (is.numeric(weights) && is.vector(weights) && length(weights) == nrow(edges))
}
graph_internal <- function(nodes, edges, weights = NULL) {  
    stopifnot(is.character(nodes), is.character(edges), is.matrix(edges), valid_weights(weights, edges))
    edges <- graph_make_edges(nodes, edges)
    graph_internal_make (nodes, edges, weights) 
}
graph_internal_make <- function(nodes, edges, weights) {
    stopifnot(is.character(nodes), is.numeric(edges), is.matrix(edges), valid_weights(weights, edges))
    fromto <- c('from', 'to')
    colnms <- colnames(edges) 
    stopifnot(length(colnms) == 0 || identical(colnms, fromto))
    colnames(edges) <- fromto
    dag <- list(nodes=nodes, edges=edges, weights = weights ) 
    class(dag) <- 'bnc_graph_internal'
    dag
}
graph_nodes <- function(x) {
  stopifnot(is( x, "bnc_graph_internal"))
  x$nodes 
}
# TODO: will use this function for conversion to graphNEL
graphNEL2_graph_internal <- function(x) { 
  stopifnot(inherits(x, "graphNEL"))
  nodes <- graph::nodes(x)
  edges <- t(named_edge_matrix(x)) 
  # Will fail if it does not have the attribute.
  weights <- graph::edgeData(self = x, from = edges[, 1], to = edges[, 2], 
                             attr = "weight") 
  weights <- unlist(weights)
  graph_internal(nodes, edges, weights ) 
}  
graph_internal2graph_NEL <- function(x) {  
  stopifnot(inherits( x, "bnc_graph_internal")) 
  edges <- graph_named_edge_matrix(x) 
  graph::ftM2graphNEL(ft = edges, W = NULL, V = x$nodes, edgemode = "directed")  
} 
graph_make_edges <- function(nodes, edges) { 
  stopifnot(is.character(nodes), is.character(edges), ncol(edges) == 2,
            all(edges[] %in% nodes))
  from <- match(edges[, 1], nodes) - 1
  to <- match(edges[, 2], nodes) - 1
  graph_from_to_to_edges (from = from, to = to)
}  
graph_from_to_to_edges <- function(from, to) { 
  matrix(c(from = from, to = to), ncol = 2)
}
call_bh <- function(fun, g, ...) { 
 do.call(fun, args = list(vertices = g$nodes, edges  = g$edges, ...)) 
}
#'  connected_components 
#'  
#'  @param  x currently a graphNEL. TODO But will be a graph_internal.
#'  @keywords internal
graph_connected_components <- function(x) {  
  g <- graphNEL2_graph_internal(x)
  stopifnot(inherits(g, "bnc_graph_internal"))  
  connected <- call_bh('bh_connected_components', g)
  comps <- split(graph_nodes(g), connected + 1)
  # TODO remove this. 
  if (length(comps) > 0) {
    comps 
  } 
  else {
    NULL
  }
} 
#'  Subgraph.  
#'  Only for a directed graph?
#'  @param  x currently a graphNEL. TODO But will be a graph_internal.
#'  @keywords internal
graph_subgraph <- function(nodes, x) { 
  g <- graphNEL2_graph_internal(x)
  stopifnot(inherits(g, "bnc_graph_internal"))   
  subgraph <- call_bh('bh_subgraph', g = g,  subgraph_vertices = nodes) 
  subgraph <- graph_internal_make(subgraph$nodes, subgraph$edges, NULL)
  # TODO remove:
  graph_internal2graph_NEL(subgraph ) 
}  
# No need to call BGL for this. 
graph_add_node <- function(node, x) { 
  g <- graphNEL2_graph_internal(x)
  stopifnot(inherits( g, "bnc_graph_internal"), is.character(node)) 
  if (node %in% g$nodes) stop("Node already in graph") 
  g$nodes <- c(g$nodes, node)  
  graph_internal2graph_NEL(g) 
}
graph_remove_node <- function(node, x) {
  g <- graphNEL2_graph_internal(x) 
  stopifnot(inherits( g, "bnc_graph_internal"), is.character(node))  
  if (!node %in% g$nodes) stop("Node not in graph")  
  removed <- call_bh('bh_remove_node', g = g, remove = node)  
  removed <- graph_internal_make(removed$nodes, removed$edges, NULL)
  graph_internal2graph_NEL(removed) 
}
#' Add edges
#' Does not allow edges among adjacent nodes
#' @keywords  internal
graph_add_edges <- function(from, to, g) {   
  g <- g 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(g) 
  }
  stopifnot(inherits( g, "bnc_graph_internal")) 
  # check from and to are disjoint and same length
  stopifnot(is.character(from),     is.character(to),
            are_disjoint(from, to), length(from) == length(to)) 
  adj <- any(graph_is_adjacent(g, from = undirected_from, to = undirected_to))
  stopifnot(!adj)
  # just simply convert the edges to numbers and then add to egisting matrig. 
  # all nodes must be already in matrig.
  edges <- graph_from_to_to_edges(from, to)
  edges <- graph_make_edges(g$nodes, edges )
  edges <- rbind(g$edges, edges)
  augmented <- graph_internal_make(nodes = g$nodes, edges = edges, NULL)  
  graph_internal2graph_NEL(augmented) 
}
#' Checks whether nodes are adjacent
#' @keywords  internal
graph_is_adjacent <- function(from, to, x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  }
  stopifnot(inherits( g, "bnc_graph_internal")) 
#   Consider both directions when checking the edges are not in graph already
  # undirected_from <- c(from, to)
  # undirected_to <- c(to, from)
  # CHeck any of these are found in the matrix
  warning("bnclassify not implemented")
  FALSE
}
graph_num_arcs <- function(x) {
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  }
  stopifnot(inherits( g, "bnc_graph_internal")) 
  nrow(g$edges)
}
graph_num_nodes <- function(x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  }
  stopifnot(inherits( g, "bnc_graph_internal")) 
  length(g$nodes)
}
graph_parents <- function(x) {  
  g <- graphNEL2_graph_internal(x)
  stopifnot(inherits( g, "bnc_graph_internal"))  
  nnodes <- graph_num_nodes(g)
  if (nnodes == 0) return(list())
  parents <- setNames(replicate(nnodes, character()), graph_nodes(g))
  if (graph_num_arcs(g) == 0) return(parents)
  edges <- graph_named_edge_matrix(g) 
  have_parents <- tapply(unname(edges[, 'from']), unname(edges[, 'to']),
                         identity, simplify = FALSE)
  parents[names(have_parents)] <- have_parents
  parents  
}
#' Returns an edge matrix with node names (instead of node indices).
#' 
#' @return A character matrix. 
#' @keywords internal
graph_named_edge_matrix <- function(x) {  
  stopifnot(inherits( x, "bnc_graph_internal"))  
  u <-  x$edges
  u[] <- x$nodes[as.vector(u) + 1]
  if (length(u) == 0) mode(u) <- 'character'
  stopifnot(is.character(u))
  u
}
graph_mstree_kruskal <- function(x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  } 
  kruskal <- call_bh('bh_mstree_kruskal', g, weights = g$weights) 
  graph_internal2graph_NEL(kruskal)  
}