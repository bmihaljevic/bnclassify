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
graph_internal <- function(nodes, edges) {  
    stopifnot(is.character(nodes), is.character(edges), is.matrix(edges))
    edges <- graph_make_edges(nodes, edges)
    graph_internal_make (nodes, edges) 
}
graph_internal_make <- function(nodes, edges) {   
    stopifnot(is.character(nodes), is.numeric(edges), is.matrix(edges))
    dag <- list(nodes=nodes, edges=edges) 
    class(dag) <- 'bnc_graph_internal'
    dag
}
graph_nodes <- function(x) {
  stopifnot(is( x, "bnc_graph_internal"))
  x$nodes 
}
graphNEL2_graph_internal <- function(x) { 
  stopifnot(inherits(x, "graphNEL"))
  nodes <- graph::nodes(x)
  # TODO: named_edge_matrix maybe should be refactored a bit
  edges <- named_edge_matrix(x)
  graph_internal(nodes, edges ) 
}  
graph_internal2graph_NEL <- function(x) {  
  stopifnot(inherits( x, "bnc_graph_internal")) 
  edges <- x$edges
  edges[] <- x$nodes[x$edges + 1]
  graph::ftM2graphNEL(ft = edges, W = NULL, V = x$nodes, edgemode = "directed")  
} 
graph_make_edges <- function(nodes, edges) { 
  stopifnot(is.character(nodes), is.character(edges), nrow(edges) == 2)
  from <- match(edges[1, ], nodes) - 1
  to <- match(edges[2, ], nodes) - 1
  edges <- matrix(c(from, to), ncol = 2)
  edges 
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
  subgraph <- graph_internal_make(subgraph$nodes, subgraph$edges)
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
  # graph::removeNode(node, g) 
  graph_internal2graph_NEL(g) 
}
graph_num_arcs <- function(x) { 
  g <- graphNEL2_graph_internal(x)
  stopifnot(inherits( g, "bnc_graph_internal")) 
  nrow(g$edges)
}