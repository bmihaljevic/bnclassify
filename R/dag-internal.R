dag_internal <- function(nodes, edges) { 
    dag <- list(nodes=nodes, edges=edges) 
    class(dag) <- 'bnc_dag_internal'
    dag
}
dag_internal2bgl <- function(dag) {
  nodes <- dag$nodes 
  # - 1 because it is 0-based
  from <- match(dag$edges[, 1], nodes) - 1
  to <- match(dag$edges[, 2], nodes) - 1
  dag$edges <- matrix(c(from, to), ncol = 2)
  dag 
}
#' Returns a naive Bayes structure
#' 
#' @keywords internal
tmp_nb_dag <- function(class, features) {
#   Check class is character and length one, features is length 0 or character,
#   class is not in features.
    check_features(features, class)
#   If > 0 features, add arc from class to each of them
    narcs <- length(features)
    arcs  <- matrix(character(narcs * 2), ncol = 2)
    if (narcs > 0) { 
      arcs <- cbind(from = class, to = features)
    }
#   Set nodes as class + features 
    nodes <- c(class, features)
    dag_internal(nodes, arcs) 
}

# TODO: the matrix list should contain integers, not strings, to avoid back and forth with boost
#     a print function could translate the integers to strings
# TODO: implement the interfac of dag.r.  
# 
# nodes()
# add ..
# is_dag() : here call  the acyclic check  
# I only need boost for algorithms and things I do not implement

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
