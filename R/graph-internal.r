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


# TODO: add checks from make_graph 

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
  is.null(weights) || (!is.null(edges) && is.numeric(weights) && is.vector(weights) && length(weights) == nrow(edges))
} 
valid_nodes <- function(nodes) {
  is.null(nodes) || is.character(nodes) 
} 
valid_edges <- function(edges, numeric = TRUE) {
  if (is.null(edges) ) return(TRUE) 
  valid <- is.matrix(edges) && (ncol(edges) == 2) && (colnames(edges) == c('from', 'to') )
  if (numeric) {
    valid <- valid && is.numeric(edges)
  }
  else { 
    valid <- valid && is.character(edges)
  }
  valid 
}  
graph_internal <- function(nodes = NULL, edges = NULL, weights = NULL, edgemode = "directed") {
    #  pass edges to numeric. Useful for BH. 
    edges <- graph_make_edges(nodes, edges)
    graph_internal_make(nodes, edges, weights, edgemode) 
}
graph_internal_make <- function(nodes, edges, weights, edgemode) { 
    stopifnot(valid_nodes(nodes), valid_edges(edges, numeric = TRUE), valid_weights(weights, edges))
    fromto <- c('from', 'to')
    colnms <- colnames(edges) 
    stopifnot(length(colnms) == 0 || identical(colnms, fromto))
    colnames(edges) <- fromto
    dag <- list(nodes=nodes, edges=edges, weights = weights, edgemode = edgemode)
    class(dag) <- 'bnc_graph_internal'
    dag
}
graph_nodes <- function(x) {
  stopifnot(is( x, "bnc_graph_internal"))
  x$nodes 
} 
graph_make_edges <- function(nodes, edges) { 
  stopifnot(valid_nodes(nodes), valid_edges(edges, numeric = FALSE), 
            all(edges[] %in% nodes), all(edges[, 1] != edges[, 2]))
  from <- match(edges[, 1], nodes) - 1
  to <- match(edges[, 2], nodes) - 1
  graph_from_to_to_edges (from = from, to = to)
}  
graph_from_to_to_edges <- function(from, to) { 
  # TODO: a hack. depending from where it is called, i might return a character or integer matrix. must specify pre and post conditions.
  if (is.null(from)) from <- character()
  if (is.null(to)) to <- character()
  m <- matrix(c(from = from, to = to), ncol = 2)
  colnames(m) <- c('from', 'to')
  m
}
call_bh <- function(fun, g, ...) { 
 do.call(fun, args = list(vertices = g$nodes, edges  = g$edges, ...)) 
}
#'  connected_components 
#'  
#'  @param  x currently a graphNEL. TODO But will be a graph_internal.
#'  @keywords internal
graph_connected_components <- function(x) {   
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
    rm(x)
  }
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
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(g) 
    rm(x)
  }
  stopifnot(inherits( g, "bnc_graph_internal"))   
  subgraph <- call_bh('bh_subgraph', g = g,  subgraph_vertices = nodes) 
  subgraph <- graph_internal_make(subgraph$nodes, subgraph$edges, NULL, g$edgemode)
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
  # TODO: currently this does not preserve node weights!!!  Yet is it not used now for weighted graphs.
  removed <- graph_internal_make(removed$nodes, removed$edges, NULL, g$edgemode)
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
  stopifnot(graph_is_directed(g)) 
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
  augmented <- graph_internal_make(nodes = g$nodes, edges = edges, NULL, "directed")  
  graph_internal2graph_NEL(augmented) 
}
graph_remove_edges <- function(from, to, g) { 
  g <- g 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(g) 
  }
  stopifnot(inherits( g, "bnc_graph_internal")) 
  # currently only for undirected
  stopifnot(graph_is_undirected(g))  
  if (!all(from %in% g$nodes)) stop("Node not in graph")  
  if (!all(to %in% g$nodes)) stop("Node not in graph")  
  removed <- call_bh('bh_remove_edges', g = g, remove_from = from, remove_to = to, edgemode = g$edgemode)  
  # TODO: currently this does not preserve node weights!!!  Yet is it not used now for weighted graphs.
  removed <- graph_internal_make(removed$nodes, removed$edges, NULL, g$edgemode)
  graph_internal2graph_NEL(removed)  
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
#' Finds adjacent nodes. Has not been tested much
#' @keywords  internal
graph_get_adjacent <- function(node, x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
    rm(x)
  } 
  stopifnot(inherits( g, "bnc_graph_internal")) 
  stopifnot(node %in% g$nodes)
  # unfortunately, node is in numbers internally. TODO: better use strings definitely. simpler.
  edges <- graph_named_edge_matrix(g)
  row_inds <- apply(edges, 1, function(g) node %in% g)
  nodes <- unique(edges[row_inds,  ] )
  setdiff(nodes, node)   
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
  if(anyNA(u)) browser()
  u
}
graph_mstree_kruskal <- function(x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  } 
  kruskal <- call_bh('bh_mstree_kruskal', g, weights = g$weights)  
  kruskal <- graph_internal_make(kruskal$nodes, kruskal$edges, kruskal$weights, "undirected")
  graph_internal2graph_NEL(kruskal)  
} 
graph_is_directed <- function(x) { 
  stopifnot(inherits( x, "bnc_graph_internal"), x$edgemode %in% c("directed", "undirected"))  
  x$edgemode == "directed" 
} 
# TODO: have not tested this one much
graph_is_connected <- function(x) { 
  stopifnot(inherits( x, "bnc_graph_internal"), x$edgemode %in% c("directed", "undirected"))  
  conn <- graph_connected_components(x) 
  # TODO Assuming  that an empty graph is connected, not sure if that is correct
  length(conn) < 2
}
graph_is_undirected <- function(x) { 
  stopifnot(inherits( x, "bnc_graph_internal"), x$edgemode %in% c("directed", "undirected"))  
  x$edgemode == "undirected" 
}
graph_empty_undirected <- function() {
  # TODO remove this function. call graph_internal directly.
  g <- graph_internal(edgemode = "undirected")
  graph_internal2graph_NEL(g) 
} 
graph_complete_undirected <- function(nodes) {
  all_pairs <- t(combn(nodes, 2))
  colnames(all_pairs) <- c('from', 'to')
  g <- graph_internal(nodes, all_pairs, NULL, "undirected")
  graph_internal2graph_NEL(g)
}
graph_max_weight_forest <- function(x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
  }
  stopifnot(inherits( g, "bnc_graph_internal"))  
  stopifnot(graph_is_undirected(g))
  if (graph_num_arcs(g) < 1) return( graph_internal2graph_NEL( g )  )
  #   change weights sign because Kruskal only searches for minimal tree 
  g$weights <-  -1 * g$weights   
  mstree <- graph_mstree_kruskal(x=g)
  mstree  
}
graph_is_dag <- function(dag) {   
  g <- dag 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(dag) 
    rm(dag)
  } 
  stopifnot(inherits( g, "bnc_graph_internal"))  
  if (!graph_is_directed(g)) return(FALSE)
  # Empty graphs are OK
  if ( graph_num_arcs(g) == 0) return(TRUE)
  tsort <- tryCatch(call_bh('bh_tsort', g), warning = function(e) character(0), 
                    error = function(e) character(0))
  length(tsort) > 0 
}   
graph_internal_union <- function(g) {
  stopifnot(is.list(g))
  g <- lapply(g, graphNEL2_graph_internal) 
  edges <- lapply(g, graph_named_edge_matrix)
  edges <- Reduce(rbind, edges)
  nodes <- sapply(g, graph_nodes)
  nodes <- as.vector(unlist(nodes))
  stopifnot(all(sort(nodes) == sort(unique(nodes))))
  # TODO: check no repeated arcs!! 
  g <- graph_internal(nodes, edges = edges, weights = NULL, edgemode = "directed")
  graph_internal2graph_NEL(g) 
}

graph_direct_tree <- function(x, root = NULL) {
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
    rm(x)
  } 
  if (graph_num_arcs(g) < 1) {
    return(graph_direct( g)) 
  }
  stopifnot(graph_is_undirected(g))  
  stopifnot(graph_is_connected(g))
  current_root <- graph_nodes(g)[1]  
  if (length(root) && root %in% graph_nodes(g)) {
#   if root is not in tree keep silent as it might be in another tree 
#   of the forest    
    current_root <- root
  }
  directed <- graph_internal(nodes=graph_nodes(g), edgemode='directed')
  direct_away_queue <- current_root
  while (length(direct_away_queue)) {
    current_root <- direct_away_queue[1]
    #   convert edges reaching current_root into arcs leaving current_root 
    adjacent <- graph_get_adjacent(current_root, g) 
    if (length(adjacent)) {      
      replicated_root <- rep(current_root, length(adjacent))
      directed <- graph_add_edges(from=replicated_root , to=adjacent, g = directed)
      g <-  graph_remove_edges(from=replicated_root, to=adjacent, g)
    }
    direct_away_queue <- direct_away_queue[-1]
    direct_away_queue <- c(direct_away_queue, adjacent)
  }   
  directed 
} 
graph_direct <- function(x) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
    rm(x)
  } 
  g$edgemode = "directed"
  graph_internal2graph_NEL (g) 
}   
graph_direct_forest <- function(x, root = NULL) { 
  g <- x 
  if (!inherits( g, "bnc_graph_internal"))  {
    g <- graphNEL2_graph_internal(x) 
    rm(x)
  }  
  if (graph_num_nodes(g) == 0L) {
    return(direct_graph(g))
  }
  if (length(root)) stopifnot(root %in% graph_nodes(g))
  components <- connected_components(g) 
  components <- lapply(components, subgraph, g)
  trees <- lapply(components, direct_tree, root)
  g <- graph_union(g = trees)  
  g
}