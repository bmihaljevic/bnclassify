# supernodes: related ..
# include_feature: include_node ...
# add_feature_parents: relate_nodes
# ===========================================================
# Returns sets of non class-conditionally independent features
not_cci <- function(x) {
  stopifnot(is_aug_nb(x))
  features <- subgraph(features(x), to_graphNEL(x))
  connected_components(features)
}
add_feature_parents <- function(parents, feature, x) {
  stopifnot(is_aug_nb(x))  
  g <- condition_on(parents, feature, to_graphNEL(x))
  bnc_dag(g, class_var(x), NULL)
}
relate_supenodes <- function(child_sn, parent_sn, x) {
#   stopifnot(is_aug_nb(x))  
#   check child is_supernode (child, x)
#   check parent is supernode  (parent, x)
#   condition_on(child_sn, parent_sn, bnc_dag(x))
  # bnc_struct(g, class=x$class_var)
}
add_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  a <- add_node(node, to_graphNEL(x))
  class <- class_var(x)
  a <- condition_on(parents = class, nodes = node, x=a)
  bnc_dag(a, class, NULL)
}
#' @export 
plot.bnc_dag <- function(x, y, layoutType='dot', ...) {
  graph::plot(to_graphNEL(x))
}
# ========================
# Type functions
is_semi_naive <- function(x) {
  warning(as.character(match.call()[[1]]), "Not implemented.")
  TRUE
}
is_aug_nb <- function(x) {
  warning(as.character(match.call()[[1]]), "Not implemented.")
  # Check call has no parents
  # Check class is in all families
  TRUE
}