#' Returns a complete unweighted graph with the given nodes.
#' 
#' @param nodes A character vector.
#' @return a \code{graphNEL} object.
#' @keywords internal
complete_graph <- function(nodes) {   
  g <- graph::graphNEL(nodes)
  gg <- graph::complement(g)
  stopifnot(gRbase::is.complete(gg))
  gg
} 
make_graph <- function(nodes, from, to, weights) {  
# Check nodes is character 
  stopifnot(is.character(nodes))
#  Make graph from nodes 
  g <- graph::graphNEL(nodes)  
#  Check lengths of from, to, weights all the same
  stopifnot(length(from) == length(to))
  stopifnot(length(to) == length(weights))
# If no edges to add, return graph 
  if (length(from) == 0)  { return (g) }    
#  Check from, to character and weights numeric
  stopifnot(is.character(from))
  stopifnot(is.character(to))
  stopifnot(is.numeric(weights))
#  Add edges from to with weights   
  g <- graph::addEdge(from = from, to = to, graph = g, weights = weights)
#  Check sum of weights in g is sum of weights
  w <- unlist(graph::edgeWeights(g))
  weights_correct <- all.equal(sum(w), sum(weights) * 2, tolerance = 1E-10)
  stopifnot(weights_correct)
# Return graph   
  g
}  
#' Returns an edge matrix with node names (instead of node indices).
#' 
#' @param g A \code{\link{graphNEL}}
#' @return A character matrix. 
#' @keywords internal
named_edge_matrix <- function(g) {
  u <- graph::edgeMatrix(g)
  u[] <- graph::nodes(g)[as.vector(u)]
  if (length(u) == 0) mode(u) <- 'character'
  stopifnot(is.character(u))
  u
}
#' Direct an undirected graph.
#' 
#' Starting from a \code{root} not, directs all arcs away from it and applies 
#' the same, recursively to its children and descendents. Produces a directed
#' forest.
#' 
#' @param g An undirected \code{\link{graphNEL}}.
#' @param root A character. Optional tree root.
#' @return A directed \code{\link{graphNEL}}.
#' @keywords internal
direct_forest <- function(g, root = NULL) {  
  if (graph::numEdges(g) < 1) return(g)
  stopifnot(!graph::isDirected(g))  
  if (length(root)) stopifnot(root %in% graph::nodes(g))
  components <- RBGL::connectedComp(g) 
  components <- lapply(components, graph::subGraph, g)
  trees <- lapply(components, direct_tree, root)
  graph_union(g = trees)  
}
#' Direct an undirected graph.
#' 
#' The graph must be connected and the function produces a directed tree. 
#' @return A \code{\link{graphNEL}}. The directed tree.
#' @keywords internal
direct_tree <- function(g, root = NULL) {
  if (graph::numEdges(g) < 1) return(g)
  stopifnot(!graph::isDirected(g))  
  stopifnot(graph::isConnected(g))
  current_root <- graph::nodes(g)[1]  
  if (length(root) && root %in% graph::nodes(g)) {
#   if root is not in tree keep silent as it might be in another tree 
#   of the forest    
    current_root <- root
  }
  directed <- graph::graphNEL(nodes=graph::nodes(g), edgemode='directed')
  direct_away_queue <- current_root
  while (length(direct_away_queue)) {
    current_root <- direct_away_queue[1]
    #   convert edges reaching current_root into arcs leaving current_root 
    adjacent <- graph::edges(g, current_root)[[1]]
    if (length(adjacent)) {      
      directed <- graph::addEdge(from=current_root, to=adjacent, directed)
      g <- graph::removeEdge(from=current_root, to=adjacent, g)
    }
    direct_away_queue <- direct_away_queue[-1]
    direct_away_queue <- c(direct_away_queue, adjacent)
  }
  directed
}
#' Returns the undirected augmenting forest.
#' 
#' Uses Kruskal's algorithm to find the augmenting forest that maximizes the sum
#' of pairwise weights. When the weights are class-conditional mutual
#' information this forest maximizes the likelihood of the tree-augmented naive
#' Bayes network.
#' 
#' If \code{g} is not connected than this will return a forest; otherwise it is 
#' a tree.
#' 
#' @param g \code{\link{graphNEL}} object. The undirected graph with pairwise 
#'   weights.
#' @return A \code{\link{graphNEL}} object. The maximum spanning forest.
#' @references Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network 
#'   classifiers. \emph{Machine Learning}, \bold{29}, pp. 131--163.
#'   
#'   Murphy KP (2012). \emph{Machine learning: a probabilistic perspective}. The
#'   MIT Press. pp. 912-914.
#' @keywords internal
max_weight_forest <- function(g) {         
  stopifnot(!graph::isDirected(g))
  if (graph::numEdges(g) < 1) return(g)
  #   change weights sign because Kruskal only searches for minimal tree
  e <- named_edge_matrix(g = g)
  weights <- graph::edgeData(self = g, from = e[1, ], to = e[2, ], 
                             attr = "weight")
  weights <- unlist(weights)
  graph::edgeData(self = g, from = e[1, ], 
                  to = e[2, ], attr = "weight") <- -1 * weights
  mstree <- RBGL::mstree.kruskal(x=g)
  #   make a graphNEL 
  gr <- graph::graphNEL(mstree$nodes)
  weights <- -1 * as.vector(mstree$weights)
  graph::addEdge(mstree$edgeList[1,], mstree$edgeList[2,], gr, weights= weights)    
}
#' Merges multiple disjoint graphs into a single one.
#' 
#' @param g A \code{\link{graphNEL}}
#' @return A \code{\link{graphNEL}}. 
#' @keywords internal
graph_union <- function(g) {
  stopifnot(is.list(g))
  edges <- lapply(g, named_edge_matrix)
  edges <- Reduce(cbind, edges)
  nodes <- sapply(g, graph::nodes)
  nodes <- as.vector(unlist(nodes))
  stopifnot(all(sort(nodes) == sort(unique(nodes))))
  union <- graph::graphNEL(nodes = nodes, edgemode = "directed")
  graph::addEdge(from = edges[1,], to = edges[2,], graph = union)
}
#' Adds a node to DAG as root and parent of all nodes.
superimpose_node <- function(dag, node) {
  check_dag(dag)
#   Check node is length one character 
  check_node(node)  
#   Check node not in dag nodes 
  nodes <- graph::nodes(dag)
  stopifnot(!(node %in% nodes))
#   Add node and edges
  graph::addNode(node = node, object = dag, edges = list(nodes))
}
#' Checks it is a valid DAG. 
check_dag <- function(dag) {
  #   Check dag is graphNEL. Allow adjacency matrix?
  stopifnot(inherits(dag, "graphNEL"))
  #   If non-empty graph, check dag is a dag. gRbase fails with empty graph.
  if (graph::numNodes(dag) > 0) { stopifnot(gRbase::is.DAG.graphNEL(dag))   }   
}
check_node <- function(node) {
  stopifnot(assertthat::is.string(node))
}
#' Returns a naive Bayes structure
#' 
#' @keywords internal
nb_dag <- function(class, features) {
#   Check class is character and length one, features is length 0 or character,
#   class is not in features.
    check_features(features, class)
#   If > 0 features, add arc from class to each of them
    narcs <- length(features)
    arcs  <- matrix(character(narcs * 2), ncol = 2)
    if (narcs > 0) { 
      arcs <- cbind(from=class, to=features)
    }
#   Set nodes as class + features 
    nodes <- c(class, features)
#    Call ftM2graphNEL 
    graph::ftM2graphNEL(ft = arcs, W=NULL, V=nodes, edgemode="directed")
}
#' Creates a random augmented NB with class as class. 
random_aug_nb_dag <- function(class, V, maxpar, wgt) {
  dg <- gRbase::random_dag(V = V, maxpar = maxpar, wgt = wgt)
  superimpose_node(dag = dg, class)
}
# #' Relates two disjoint, disconnected sets of nodes.
# #' 
# #' Inserts arcs from all nodes in \code{a} to all nodes in \code{b}.
# #' 
# #' If \code{complete == TRUE}, the node sets must be complete, i.e. there must 
# #' be an arc between each of their elements (useful for semi-naive Bayes).
# #' 
# #' @param a A character vector.
# #' @param b A character vector.
# #' @param g A \code{graphNEL} directed acyclic graph.
# #' @return A \code{graphNEL} directed acyclic graph.
# #' @return logical Whether to check \code{a}, \code{b}, and their created 
# #'   subgraph are complete.
# #' @keywords internal
# relate_nodes_in_graph <- function(a, b, g, complete=TRUE) {
#   #   Maybe there is a more efficient implementation of the checks(e.g. igraph) to 
#   #   use instead.  
#   stopifnot(gRbase::is.DAG(g))
#   stopifnot(!length(intersect(a, b)))
#   ug <- graph::ugraph(g)
#   if (complete) {
#     stopifnot(gRbase::is.complete(ug, a))
#     stopifnot(gRbase::is.complete(ug, b))    
#   }
#   #   Make sure no arc between them.   
#   sapply(b, function(n) {
#     neigh <- any(graph::isAdjacent(ug, from = a, to = n))  
#     if (neigh) stop("Nodes cannot be neighbors")
#     NULL
#   })
#   rm(ug)  
#   sapply(b, function(n) {
#     g <<- graph::addEdge(from=a, to=n, graph=g)
#     NULL
#   })
#   if (complete) {
#     ug <- graph::ugraph(g)  
#     stopifnot(gRbase::is.complete(ug, c(a, b)))
#   }  
#   stopifnot(gRbase::is.DAG(g))
#   g
# }
# #' Removes edges with weight below \code{alpha}
# #' 
# #' @param g a \code{graphNEL} object 
# #' @param alpha numeric 
# filter_edges_by_attr <- function(attr, g, comparison, alpha) {
#   stopifnot(length(attr) > 0)
#   stopifnot(!graph::isDirected(g))
#   stopifnot(is.numeric(alpha))
#   stopifnot(comparison %in% c('<', '>', '<=', '>=', '=='))    
#   remove <- NULL
#   em <- named_edge_matrix(g)
#   if (length(em) > 0) {    
#     ew <- unlist(graph::edgeData(g, from = em[1, ], to = em[2, ], attr= attr))  
#     remove_ind <- do.call(comparison, args=list(ew, alpha))
#     remove <- em[, remove_ind, drop=T]  
#     g <- graph::removeEdge(from=remove[1,], to=remove[2,], graph=g)
#   }
#   list(filtered=g, removed=remove)  
# }