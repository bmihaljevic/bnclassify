#' Returns a Superparent one-dependence estimator. 
#' 
#' @param sp character The superparent.
#' @keywords internal
spode <- function(sp, features, class) {
  stopifnot(length(sp) == 1)
  stopifnot(length(class) == 1)
  stopifnot(class != sp)
  stopifnot(!class %in% features)
  features <- setdiff(features, sp)
  features_graph <- nb_dag(class = sp, features = features)
  dag <- superimpose_node(node = class, dag = features_graph) 
  bnc_dag(dag = dag, class = class)
} 