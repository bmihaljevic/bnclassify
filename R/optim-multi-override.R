multi_update <- function(x, dataset, dag, smooth = NULL) {
  # The lp args must be either smooth or within the x objects: 
  if (is.null(smooth)) {
    x <- ensure_list(x, type = "bnc_bn")  
  }
  else {
    x <- ensure_list(x)  
  }
  dags <- NULL
  if (dag) {
    dags <- lapply(x, update_dag, dataset)
  }
  if (is.null(dags)) {
    dags <- lapply(x, bn2dag)
  }
  # smooth overrides lp args that may be in x 
  if (!is.null(smooth)) {
    multi_bnc_bn(dags, dataset, smooth = smooth)
  }
  else {
    lp_multi_args <- lapply(x, get_lp_multi_update_args)
    # If all lp args contain just smooth, and the smooth value is identical, 
    # then can call multi_bnc_bn to take advantage of shared CPTs
    if (are_all_equal(lp_multi_args) && identical(names(lp_multi_args[[1]]), 'smooth')) {
      multi_bnc_bn(dags, dataset, smooth = lp_multi_args[[1]]$smooth)
    }
    else {
      lp_args <- lapply(x, get_lp_update_args)
      mapply(update_lp, dags, lp_args, MoreArgs = list(dataset = dataset), 
             SIMPLIFY = FALSE)
    }  
  }
}
multi_bnc_bn <- function(x, dataset, smooth) {
  # Unnamed so that it would pass no names to objects created by itearting on it
  x <- ensure_multi_list(x)
  # Check bnc dag
  # lapply(x, check_bnc_dag)
  # Check the class is common to all data sets
  class <- get_common_class(x)
  check_class_in_dataset(class, dataset)
  ucpts <- extract_unique_cpts(x, dataset, smooth)
  params_list <- lapply(x, extract_params_cptpool, ucpts)
  # Make a bnc_bn for each x
  bnc_bns <- mapply(make_bnc_bn, x,  params_list, SIMPLIFY = FALSE)
  # lapply(bnc_bns, check_bnc_bn)
  bnc_bns
}
extract_params_cptpool <- function(x, cpt_pool) {
  # Match families to CPTS 
  fams_ids <- make_families_ids(families(x))
  # The following line could be extracted to calling function for speed-up
  cpts_ids <- make_families_ids(lapply(cpt_pool, cpt2family))
  cpt_pool[match(fams_ids, cpts_ids)]
}
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