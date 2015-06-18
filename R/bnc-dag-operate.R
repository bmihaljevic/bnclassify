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
  bnc_dag(g, class_var(x))
}
# Just a convenience for calling add_feature_parents from *ply loops
add_feature_children <- function(feature, parents, x) {
  add_feature_parents(parents, feature, x)
}
relate_supernodes <- function(child_sn, parent_sn, x) {
  stopifnot(is_aug_nb(x))  
#   check child and parent are supernodes 
  stopifnot(is_supernode(child_sn, x), is_supernode(parent_sn, x))
  g <- condition_on(parent_sn, child_sn, to_graphNEL(x))
  bnc_dag(g, class_var(x))
}
add_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  a <- add_node(node, to_graphNEL(x))
  class <- class_var(x)
  a <- condition_on(parents = class, nodes = node, x = a)
  bnc_dag(a, class)
}
remove_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  g <- remove_node(node, to_graphNEL(x))
  bnc_dag(g, class_var(x))
}
narcs <- function(x) {
  num_arcs(to_graphNEL(x))
}
#' @export 
plot.bnc_dag <- function(x, y, layoutType='dot', ...) {
  graph::plot(to_graphNEL(x))
}
is_supernode <- function(node, x) {
  warning("Not implemented")
  TRUE
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
is_ode <- function(x) {
  warning(as.character(match.call()[[1]]), "Not implemented.")
  TRUE
}
# ========================
# Keogh
feature_orphans <- function(bnc_dag) {
  # Get the family of each feature 
  fams <- feature_families(bnc_dag)
  # Get those features whose family is of size 2 (itself and class)
  ind_orphans <- (vapply(fams, length, FUN.VALUE = integer(1)) == 2)
  fams <- fams[ind_orphans]
  if (length(fams) == 0) return(NULL)
  #  ... check they effectively are themselves and class
  feats <- features(bnc_dag)[ind_orphans]
  fams_ok <- mapply(is_orphan_fam, fams, feats, 
                    MoreArgs = list(class = class_var(bnc_dag)), 
                    SIMPLIFY = TRUE)
  stopifnot(fams_ok)                      
  # return features
  feats
}
is_orphan_fam <- function(fam, feat, class) {
  identical(fam, c(feat, class))
}