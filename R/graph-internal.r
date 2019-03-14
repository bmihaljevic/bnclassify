# all functions with begin with graph_ 
# TODO: make BH use character nodes an input and not rely on integers. It could be faster. 

new_graph_internal <- function(nodes, edges, weights, edgemode) { 
    stopifnot(valid_nodes(nodes), valid_edges(edges, numeric = FALSE), 
              all(edges[] %in% nodes), all(edges[, 1] != edges[, 2]),
              valid_weights(weights, edges))
    fromto <- c('from', 'to')
    colnms <- colnames(edges) 
    stopifnot(length(colnms) == 0 || identical(colnms, fromto))
    colnames(edges) <- fromto
    # Fixing the type. 
    if (is.null(nodes)) {
      nodes <- character()
    }
    if (is.null(weights)) {
      weights <- numeric()
    }
    dag <- list(nodes=nodes, edges=edges, weights = weights, edgemode = edgemode)
    class(dag) <- 'bnc_graph_internal'
    dag
}
graph_internal <- function(nodes = NULL, edges = NULL, weights = NULL, edgemode = "directed") {
    from <- edges[, 1]
    to <- edges[, 2]
    edges <- graph_from_to_to_edges (from = from, to = to)
    new_graph_internal(nodes, edges, weights, edgemode) 
} 
valid_weights <- function(weights, edges) {
  is.null(weights) || (!is.null(edges) && is.numeric(weights) && is.vector(weights) && length(weights) == nrow(edges))
} 
valid_nodes <- function(nodes) {
  is.null(nodes) || is.character(nodes) 
} 
valid_edges <- function(edges, numeric = TRUE) {
  if (is.null(edges) ) return(TRUE) 
  valid <- is.matrix(edges) && (ncol(edges) == 2) && (all(colnames(edges) == c('from', 'to')) )
  if (numeric) {
    valid <- valid && is.numeric(edges)
  }
  else { 
    valid <- valid && is.character(edges)
  }
  valid 
}   
graph_nodes <- function(x) {
  if (!inherits( x, "bnc_graph_internal")) stop()
  x$nodes 
}  
# Maps edgs to int 
graph_edges2int <- function(nodes, edges) { 
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
 edges <- graph_edges2int(g$nodes, g$edges)
 do.call(fun, args = list(vertices = g$nodes, edges  = edges, ...)) 
}
#' connected_components 
#'  
#' @param x currently a graphNEL. TODO But will be a graph_internal.
#' @keywords internal
graph_connected_components <- function(g) {
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
#' Subgraph.  
#' Only for a directed graph?
#' 
#' @param nodes character 
#' @param  x currently a graphNEL. TODO But will be a graph_internal.
#' @keywords internal
graph_subgraph <- function(nodes, g) {   
  stopifnot(inherits( g, "bnc_graph_internal"))   
  subgraph <- call_bh('bh_subgraph', g = g,  subgraph_vertices = nodes) 
  subgraph <- graph_internal(subgraph$nodes, subgraph$edges, NULL, g$edgemode)
  subgraph  
}  
# No need to call BGL for this. 
graph_add_node <- function(node, g) {   
  stopifnot(inherits( g, "bnc_graph_internal"), is.character(node)) 
  if (node %in% g$nodes) stop("Node already in graph") 
  g$nodes <- c(g$nodes, node)  
  g 
}
graph_remove_node <- function(node, g) {
  stopifnot(inherits( g, "bnc_graph_internal"), is.character(node))  
  if (!node %in% g$nodes) stop("Node not in graph")  
  removed <- call_bh('bh_remove_node', g = g, remove = node)  
  # TODO: currently this does not preserve node weights!!!  Yet is it not used now for weighted graphs.
  removed <- graph_internal(removed$nodes, removed$edges, NULL, g$edgemode)
  removed 
}
#' Add edges
#' Does not allow edges among adjacent nodes
#' @keywords  internal
graph_add_edges <- function(from, to, g) { 
  stopifnot(inherits( g, "bnc_graph_internal")) 
  stopifnot(graph_is_directed(g)) 
  # check from and to are disjoint and same length
  stopifnot(is.character(from),     is.character(to),
            are_disjoint(from, to), length(from) == length(to)) 
  adj <- any(graph_is_adjacent(g, from = from, to = to))
  stopifnot(!adj)
  # just simply convert the edges to numbers and then add to egisting matrig. 
  # all nodes must be already in matrig.
  edges <- graph_from_to_to_edges(from, to)
  edges <- rbind(g$edges, edges)
  augmented <- new_graph_internal(nodes = g$nodes, edges = edges, NULL, "directed")  
  augmented 
} 
graph_remove_edges <- function(from, to, g) {  
  stopifnot(inherits( g, "bnc_graph_internal")) 
  # currently only for undirected
  stopifnot(graph_is_undirected(g))  
  if (!all(from %in% g$nodes)) stop("Node not in graph")  
  if (!all(to %in% g$nodes)) stop("Node not in graph")  
  removed <- call_bh('bh_remove_edges', g = g, remove_from = from, remove_to = to, edgemode = g$edgemode)  
  # TODO: FIX THIS IS RCPP code! It might return a vector  of edges, not a matrix
  if (!is.matrix(removed$edges)) {
    if (length(removed$edges) == 2) {
      removed$edges <- matrix(removed$edges, ncol = 2)
    } 
    else {
      stop("wrong edges")
    }
  }
  # TODO: currently this does not preserve node weights!!!  Yet is it not used now for weighted graphs.
  removed <- graph_internal(removed$nodes, removed$edges, NULL, g$edgemode)
  removed  
}  
#' Checks whether nodes are adjacent
#' @keywords  internal
graph_is_adjacent <- function(from, to, g) {  
  stopifnot(inherits( g, "bnc_graph_internal")) 
#   Consider both directions when checking the edges are not in graph already
  # undirected_from <- c(from, to)
  # undirected_to <- c(to, from)
  # CHeck any of these are found in the matrix
  # warning("bnclassify not implemented")
  FALSE
} 
#' Finds adjacent nodes. Has not been tested much
#' @keywords  internal
graph_get_adjacent <- function(node, g) {  
  stopifnot(inherits( g, "bnc_graph_internal")) 
  stopifnot(node %in% g$nodes)
  row_inds <- apply(g$edges, 1, function(g) node %in% g)
  nodes <- unique(g$edges[row_inds,  ] )
  setdiff(nodes, node)   
}    
graph_num_arcs <- function(g) {  
  if (!inherits(g, "bnc_graph_internal")) stop()
  nrow(g$edges)
}
graph_num_nodes <- function(g) {
  if (!inherits(g, "bnc_graph_internal")) stop()
  length(g$nodes)
}
graph_parents <- function(g) {  
  if (!inherits(g, "bnc_graph_internal")) stop()
  nnodes <- graph_num_nodes(g)
  if (nnodes == 0) return(list())
  if (graph_num_arcs(g) == 0) {
    parents <- setNames(replicate(nnodes, character()), graph_nodes(g))
    return(parents)
  }
  families <- lapply(graph_nodes(g), graph_node_parents, g)
  setNames(families, nm = graph_nodes(g) ) 
} 
# Does not check whether node is actually in g
graph_node_parents <- function(node, g) { 
  if (!inherits(g, "bnc_graph_internal")) stop()
  if (!(in_rcpp(node, g$nodes))) stop(paste0("Vertex" , node, "is not in the graph."))
  edges <- g$edges 
  ind <- edges[, 'to'] == node
  # ind2 <- graph_node_parents_inds(edges, node)
  # if (!all.equal(ind, ind2)) browser()
  unname(edges[ind, 'from'])
}
# TODO remove this function.
graph_families <- function(g) {  
  stopifnot(inherits( g, "bnc_graph_internal"))  
  parents <- graph_parents(g)
  mapply(c, names(parents), parents, SIMPLIFY = FALSE)
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
  if(anyNA(u)) stop('NAs in graph')
  u
}
graph_mstree_kruskal <- function(g) {  
  stopifnot(inherits( g, "bnc_graph_internal"))  
  kruskal <- call_bh('bh_mstree_kruskal', g, weights = g$weights)  
  kruskal <- graph_internal(kruskal$nodes, kruskal$edges, kruskal$weights, "undirected")
  kruskal  
} 
graph_is_directed <- function(x) { 
  if (skip_assert( )) return (TRUE)
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
  g 
} 
graph_complete_undirected <- function(nodes) {
  all_pairs <- t(combn(nodes, 2))
  colnames(all_pairs) <- c('from', 'to')
  g <- graph_internal(nodes, all_pairs, NULL, "undirected")
  g
}
graph_max_weight_forest <- function(g) {  
  stopifnot(inherits( g, "bnc_graph_internal"))  
  stopifnot(graph_is_undirected(g))
  if (graph_num_arcs(g) < 1) return(  g  )
  #   change weights sign because Kruskal only searches for minimal tree 
  g$weights <-  -1 * g$weights   
  mstree <- graph_mstree_kruskal(g)
  mstree  
}
graph_is_dag <- function(g) {    
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
  edges <- lapply(g, '[[', "edges")
  edges <- Reduce(rbind, edges)
  nodes <- sapply(g, graph_nodes)
  nodes <- as.vector(unlist(nodes))
  stopifnot(all(sort(nodes) == sort(unique(nodes))))
  # TODO: check no repeated arcs!! 
  g <- graph_internal(nodes, edges = edges, weights = NULL, edgemode = "directed")
  g 
}

graph_direct_tree <- function(g, root = NULL) { 
  stopifnot(inherits( g, "bnc_graph_internal"))  
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
graph_direct <- function(g) {   
  stopifnot(inherits( g, "bnc_graph_internal"))  
  g$edgemode = "directed"
  g 
}   
graph_direct_forest <- function(g, root = NULL) {   
  stopifnot(inherits( g, "bnc_graph_internal"))  
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
graph_superimpose_node <- function(g, node) {
  stopifnot(inherits( g, "bnc_graph_internal"))
  # stopifnot(is_dag_graph(g))
#   Check node is length one character 
  check_node(node)  
#   Check node not in g nodes 
  nodes <- graph_nodes(g)
  stopifnot(!(node %in% nodes))
#   Add node and edges
  g <- graph_add_node(node = node, g)
  replicated_node <- rep(node, length(nodes))
  graph_add_edges(replicated_node, nodes, g)
}

make_graph <- function(nodes, from, to, weights) {       
  edges <- graph_from_to_to_edges(from, to) 
  # TODO: change name to make_ugraph
  g <- graph_internal(nodes, edges, weights, "undirected") 
  g
}  
# This function is also a hack. I guess weights should be already stored with edge names,
# just like edges should be stored with names. Using int ids makes no sense 
graph_get_named_weights <- function(g) { 
  stopifnot(inherits( g, "bnc_graph_internal"))  
  df <- data.frame(g$edges)
  df$w <- g$weights
  df
}