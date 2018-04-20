
#' Direct an undirected graph.
#' 
#' Starting from a \code{root} not, directs all arcs away from it and applies 
#' the same, recursively to its children and descendants. Produces a directed
#' forest.
#' 
#' @param g An undirected \code{\link{graphNEL}}.
#' @param root A character. Optional tree root.
#' @return A directed \code{\link{graphNEL}}.
#' @keywords internal
direct_forest <- function(g, root = NULL) {  
  # if (graph::numNodes(g) == 0L) {
  #   return(direct_graph(g))
  # }
  # if (length(root)) stopifnot(root %in% graph::nodes(g))
  # components <- connected_components(g) 
  # components <- lapply(components, subgraph, g)
  # trees <- lapply(components, direct_tree, root)
  # graph_union(g = trees)  
  graph_direct_forest(g, root = NULL)
}
#' Direct an undirected graph.
#' 
#' The graph must be connected and the function produces a directed tree. 
#' @return A \code{\link{graphNEL}}. The directed tree.
#' @keywords internal
direct_tree <- function(g, root = NULL) { 
  graph_direct_tree(g, root) 
}
direct_graph <- function(g) { 
  graph_direct(g)
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
  graph_max_weight_forest(g)
}
#' Merges multiple disjoint graphs into a single one.
#' 
#' @param g A \code{\link{graphNEL}}
#' @return A \code{\link{graphNEL}}. 
#' @keywords internal
graph_union <- function(g) { 
  graph_internal_union(g)
}
# Adds a node to DAG as root and parent of all nodes.
superimpose_node <- function(dag, node) {  
  graph_superimpose_node(dag, node) 
}
is_dag_graph <- function(dag) {  
  graph_is_dag(dag)
} 
check_node <- function(node) {
  stopifnot(assertthat::is.string(node))
}
#' Returns a naive Bayes structure
#' 
#' @keywords internal
nb_dag <- function(class, features) { 
 anb_make_nb(class, features)  
}