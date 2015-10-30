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