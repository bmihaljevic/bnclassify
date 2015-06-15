get_candidate_features <- function(bnc_dag, all_features) {
  setdiff(all_features, bnc_features(bnc_dag))
}
# Forms dags by conditioninng each not-included feature on each of the supernodes
includes_by_joins <- function(bnc_dag, all_features) {
  stopifnot(is_semi_naive(bnc_dag))
  to_include <- get_candidate_features(bnc_dag, all_features)
  # Nothing to do if there are no features in the dag
  if (length(to_include) == 0) return(NULL)  
  # I get the supernodes here for efficiency (to not repeat it inside each call)
  supernodes <- not_cci(x = bnc_dag)    
  l <- lapply(to_include, expand_supernodes, bnc_dag, supernodes)
  # Do I need names? With is use.names = FALSE it is faster.
  unlist(l, recursive = FALSE, use.names = FALSE)
}
expand_supernodes <- function(new_node, bnc_dag, supernodes) {
  stopifnot(is_just(supernodes, "list"))
  augmented <- add_feature(new_node, bnc_dag)
  dags <- lapply(supernodes, add_feature_parents, new_node, augmented)
  stopifnot(all(vapply(dags, is_semi_naive, FUN.VALUE = logical(1))))
  dags
}