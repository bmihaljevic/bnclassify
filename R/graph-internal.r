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
    # TODO: call to internal here?
    dag <- list(nodes=nodes, edges=edges) 
    class(dag) <- 'bnc_graph_internal'
    dag
}
graph_internal2bgl <- function(dag) {
  nodes <- dag$nodes 
  # - 1 because it is 0-based
  from <- match(dag$edges[, 1], nodes) - 1
  to <- match(dag$edges[, 2], nodes) - 1
  dag$edges <- matrix(c(from, to), ncol = 2)
  dag 
} 
connected_components <- function(x) {
  ux <- graph::ugraph(x)
  comps <- RBGL::connectedComp(ux)  
  if (length(comps) > 0) {
    comps 
  } 
  else {
    NULL
  }
}