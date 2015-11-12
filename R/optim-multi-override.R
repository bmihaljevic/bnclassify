make_cpts_ids <- function(cpts) {
  fams <- cpts2families(cpts)
  make_families_ids(fams)
}
feature_params <- function(x) {
  params(x)[features(x)]
}
multi_predict <- function(object, newdata, prob = FALSE) {
  #   if complete, then all one together
  if (!anyNA(newdata)) {
    p <- multi_compute_log_joint_per_class(object, newdata)
    p <- lapply(p, log_normalize)
    stopifnot(all(vapply(p, are_pdists, FUN.VALUE = logical(1))))
    if (prob) {
      p
    }
    else {
      lapply(p, map)
    }
  }
  #   otherwise get posterior for each separately
  else {
    lapply(object, predict, newdata,  prob = prob)
  }  
}
multi_compute_log_joint_per_class <- function(x, dataset) {
  # Make sure x is a list 
  x <- ensure_multi_list(x)
  # Get feature cpts of each x 
  feature_cpts <- lapply(x, feature_params)
  feature_cpts_ids <- lapply(feature_cpts, make_cpts_ids)
  # Flatten cpts and ids 
  feature_cpts_flat <- unlist(feature_cpts, recursive = FALSE, use.names = FALSE)
  # ..Make sure the CPTs are named with their corresponding variable
  names(feature_cpts_flat) <- unlist(lapply(feature_cpts, names), use.names = FALSE)
  stopifnot(is.list(feature_cpts_flat))
  feature_cpts_ids_flat <- unlist(feature_cpts_ids, recursive = FALSE, 
                                  use.names = FALSE)
  stopifnot(is.character(feature_cpts_ids_flat))
  # Identify unique feature cpts
  unique_feat_cpt_ids_inds <- !duplicated(feature_cpts_ids_flat)
  unique_feat_cpts_ids <- feature_cpts_ids_flat[unique_feat_cpt_ids_inds]
  unique_feat_cpts <- feature_cpts_flat[unique_feat_cpt_ids_inds]
  # Log class prior and x CPTs
  class <- get_common_class(x)
  cp <- log(params(x[[1]])[[class]])
  unique_feat_cpts[] <- lapply(unique_feat_cpts, log)
  # Convert data to cpt indices 
  cpt_inds <- x2cpt_inds(unique_feat_cpts, dataset, class, length(cp))
  # Get probs for unique feature cpts 
  N <- nrow(dataset)
  u_feat_cpt_entries <- lapply(unique_feat_cpts, get_x_cpt_entries, cpt_inds,
                               classes(x[[1]]), N)
  # Map unique CPTs back to the dags.
  dag_cpt_inds <- lapply(feature_cpts_ids, match, unique_feat_cpts_ids)
  dag_cpts <- lapply(dag_cpt_inds, function(cpt_ids) u_feat_cpt_entries[cpt_ids])
  # For each dag, log_multiply P(C) with P(x | C)
  lapply(dag_cpts, compute_class_posterior, cp, N) 
}