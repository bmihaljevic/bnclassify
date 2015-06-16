get_candidate_features <- function(bnc_dag, features_to_include) {
  setdiff(features_to_include, features(bnc_dag))
}
# Forms dags by conditioninng each not-included feature on each of the supernodes
includes_in_supernodes <- function(bnc_dag, features_to_include) {
  stopifnot(is_semi_naive(bnc_dag))
  to_include <- get_candidate_features(bnc_dag, features_to_include)
  # Nothing to do if there are no features in the dag
  if (length(to_include) == 0) return(NULL)  
  # Get the supernodes here for efficiency (to not repeat it inside each call)
  supernodes <- not_cci(x = bnc_dag)    
  l <- lapply(to_include, augment_supernodes, bnc_dag, supernodes)
  # Do I need names? With is use.names = FALSE it is faster.
  unlist(l, recursive = FALSE, use.names = FALSE)
}
augment_supernodes <- function(new_node, bnc_dag, supernodes) {
  if (length(supernodes) == 0) return(list())
  stopifnot(is_just(supernodes, "list"))
  augmented <- add_feature(new_node, bnc_dag)
  dags <- lapply(supernodes, add_feature_parents, new_node, augmented)
  stopifnot(all(vapply(dags, is_semi_naive, FUN.VALUE = logical(1))))
  dags
}
merge_supernodes <- function(bnc_dag) {
  stopifnot(is_semi_naive(bnc_dag))
  # if (length(features(bnc_dag)) == 0) return(NULL) The check below should work
  supernodes <- not_cci(bnc_dag)
  # No pairs   
  if (length(supernodes) < 2) return(NULL)
  pairs <- combn(seq_along(supernodes), 2)
  child <- supernodes[pairs[1, ]]
  parent <- supernodes[pairs[2, ]]
  dags <- mapply(relate_supernodes, child, parent, MoreArgs = list(x = bnc_dag),
                 SIMPLIFY = FALSE)
  stopifnot(all(vapply(dags, is_semi_naive, FUN.VALUE = logical(1))))  
  dags
}
includes <- function(bnc_dag, features_to_include) {
  to_include <- get_candidate_features(bnc_dag, features_to_include)
  lapply(to_include, add_feature, bnc_dag)
}
excludes <- function(bnc_dag) {
  lapply(features(bnc_dag), remove_feature, bnc_dag)  
}
fssj_step <- function(bnc_dag, features_to_include) {
  append(includes(bnc_dag, features_to_include), 
         includes_in_supernodes(bnc_dag,features_to_include))
}
bsej_step <- function(bnc_dag, ...) {
  c(excludes(bnc_dag), 
    merge_supernodes(bnc_dag))
}