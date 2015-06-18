multi_compute_augnb_luccpx <- function(x, dataset) {
  x <- ensure_list(x)
  class <- get_common_class(x)
  ucpts <- get_unique_cpts(x)
  names(ucpts) <- vapply(ucpts, get_cpt_id, FUN.VALUE = character(1))
  ind_class <- which(class == names(ucpts))
  stopifnot(assertthat::is.count(ind_class))
  uxcpts <- ucpts[-ind_class]
  cp <- ucpts[[ind_class]] 
  rm(ucpts) # Not needed any more
  factors <- get_ccx_factors(uxcpts, dataset, 
                             class, classes = cpt_1d_values(cp))
  factors[[class]] <- make_cp_factor(cp, dataset)
  dag_fams_ids <- lapply(x, families_ids)
  factors_list <- lapply(dag_fams_ids, function(cpt_ids) factors[cpt_ids] )
  lapply(factors_list, sum_log_factors)
}
compute_augnb_lucp_multi <- function(class, xfams_id_dags, unique_xcpts, cp,
                                     dataset) {
  stopifnot(is_just(xfams_id_dags, "list"))
  stopifnot(is_non_empty_complete(cp))
  cp_factor <- make_cp_factor(cp, dataset)
  # Now get the probabilities for each unique CPT. TODO: length 0 works?
  xp <- get_ccx_factors(unique_xcpts, dataset, class, 
                        classes = get_cpt_values(cp)[[1]])
  # for each list_of_cpts (dag), get the correct factors
  factors_list <- lapply(xfams_id_dags, function(fams_dag) {
    append(list(class = cp_factor), xp[fams_dag])
  })
  lapply(factors_list, sum_log_factors)
}
# Computes the 
compute_augnb_luccpx <- function(x, dataset) {
# X must be an aug nb and data must be complete for this factorization to work
# ...TODO: check for aug nb
  stopifnot(!anyNA(dataset))
  cp <- params(x)[[class_var(x)]]
  wcp <- make_cp_factor(cp, dataset) 
#  Add class posterior to factors list
  factors <- list(class = wcp)
# If there are features, get the class conditional probabilities
  features <- features(x)
  if (length(features) > 0) {
    cptsx <- params(x)[features]
    xp <- get_ccx_factors(cptsx, dataset, class_var(x), names(cp))
    factors <- append(factors, xp)
  }
#   Multiply factors
  sum_log_factors(factors)
}
get_ccx_factors <- function(cptsx, dataset, class, classes) {
  # Return empty list if no cpts
  if (length(cptsx) == 0) return( list() )
  # Get variables (1D of cpts)
  features <- vapply(cptsx, cpt_1d_var, FUN.VALUE = character(1))
  #   Get x CPT indices replicated for each class
  # warning("check dataset levels not implemented.")    
  nclass <- length(classes)
  indscx <- make_xcpt_indices(features, class, nclass, dataset)
  # Fetch from CPTs
  xp <- lapply(cptsx, subset_cpt, indscx)
  # Reshape entries to have a column per class
  n <- nrow(dataset)
  for (i in seq_along(xp)) {
    dim(xp[[i]]) <- c(n, nclass)
    colnames(xp[[i]]) <- classes
  }
  xp
}
# Make a copy of class posterior per data point
make_cp_factor <- function(cp, dataset) {
  nclass <- length(cp)
  n <- nrow(dataset)
  matrix(rep(cp, each = n), ncol = nclass, dimnames = list(NULL, names(cp)))
}
# Makes the indices to get values for observations of CPT variables and each
# possible class
make_xcpt_indices <- function(features, class, nclass, dataset) {
  # Check vars character and complete
  stopifnot(is.character(features), is_non_empty_complete(features))
  # Check data set is data.frame, with more than 0 rows
  stopifnot(is.data.frame(dataset))
  # Get obs of features from dataset, replicate nclass times as integer
  stopifnot(is.integer(nclass), nclass > 0)
  indices <- lapply(dataset[features], rep_factor_as_int, nclass)
  names(indices) <- features
  # For 1:nclass, replicate each n times; add to indices
  indices[[class]] <- rep(1:nclass, each = nrow(dataset))
  # Return indices
  indices
}
# #' Check that levels in dataset are identical to those in the CPTs
# check_dataset_levels <- function(cpts, dataset) {  
#   # Get feature levels in dataset
#   features <- names(cpts)
#   dataset <- trim_dataset(features, dataset)
#   levels <- extract_var_levels(dataset)
#   # Get the values of each feature. 
#   values <- lapply(lapply(cpts, cpt_1d_values), function(v) v$values)
#   # Check that they match the dataset. If not, put informative message   
#   stopifnot(identical(values, levels))
# }