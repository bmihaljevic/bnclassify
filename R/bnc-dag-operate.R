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
relate_supernodes <- function(child_sn, parent_sn, x) {
  stopifnot(is_aug_nb(x))  
#   check child and parent are supernodes 
  stopifnot(is_supernode(child_sn, x), is_supernode(parent_sn, x))
  g <- condition_on(parent_sn, child_sn, to_graphNEL(x))
  bnc_dag(g, class_var(x), NULL)
}
add_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  a <- add_node(node, to_graphNEL(x))
  class <- class_var(x)
  a <- condition_on(parents = class, nodes = node, x = a)
  bnc_dag(a, class, NULL)
}
remove_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  g <- remove_node(node, to_graphNEL(x))
  bnc_dag(g, class_var(x), NULL)
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

# ========================
# Keogh

#' Returns augmenting arcs that do not invalidate the ODE. 
#' 
#' @keywords internal
#' @return a character matrix. NULL if not arcs can be added.
get_aug_ode_arcs <- function(bnc_dag) {
  features <- features(bnc_dag)  
  if (length(features) < 2) return(NULL) # No pairs   
  orphans <- tan_orphans(x = bnc_dag)  
  stopifnot(length(orphans) >= 1) 
  if (length(orphans) == 1) return(NULL)
  pairs <- lapply(features, augmenting_ode_arcs_node, features, orphans, x)
  names(pairs) <- features
  if (!length(unlist(pairs))) return(NULL)
  g <- graph::graphNEL(features, edgeL=pairs, edgemode='directed')
  named_edge_matrix(g)  
}

#' Lists 'TAN orphans'.
#' 
#' @references Koegh E and Pazzani M (2002).Learning the structure of augmented 
#'   Bayesian classifiers. In \emph{International Journal on Artificial
#'   Intelligence Tools}, \bold{11}(4), pp. 587-601.
#' @keywords internal
# tan_orphans <- function(x) {

# Features conditioned only on the class
cci <- function(x) {
  stopifnot(is_aug_nb(x))  
  fams <- feature_families(x)
  feature_fams <- lapply(fams, family_features, class_var(x))
  fam_size <- vapply(feature_fams, length) 
  stopifnot(all(fam_size > 0))
  orphans <- names(fams)[fam_size == 1]
  stopifnot(all(orhans %in% features(x)))
  orphans
}
#' Returns augmenting arcs from node that do not invalidate the ODE. 
#' 
#' @keywords internal
augmenting_ode_arcs_node <- function(node, features, orphans, g) {  
  if (length(orphans) <= 1) return(NULL)
  feats_split <- split_vector(node, features)
  feats_preceding <- feats_split$pre
  feats_following <- feats_split$post
  feats_preceding <- intersect(feats_preceding, orphans)
  feats_following <- intersect(feats_following, orphans)
  to <- NULL
  # If not orphan, then not added an incoming arc from preceding features. Make 
  # it parent of preceding:
  if (!node %in% orphans && length(feats_preceding)) to <- feats_preceding
  if (length(feats_following)) to <- c(to, feats_following)
  # Finally, make sure no cycles are introduced
  setdiff(to, gRbase::ancestors(node, g))  
}
