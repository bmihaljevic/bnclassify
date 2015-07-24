#' Computes the log joint probability of the observed features for each of the
#' classes.
#' 
#' Assumes that x is an augmented naive Bayes and that data is complete.
#' 
#' If there are no features, dataset should be N x 0 dim.
#' @return N x nclass numeric matrix .
#' @keywords internal
compute_anb_log_joint_per_class <- function(x, dataset) {
#   if not complete cannot apply factorization 
  stopifnot(!anyNA(dataset))
  class <- class_var(x)
#   if not anb cannot apply factorization
  check_anb_families(families(x), class) 
  N <- nrow(dataset)
  x_cpts <- params(x)[features(x)]
  classes <- classes(x)
  cpt_inds <- x2cpt_inds(x_cpts, dataset, class, length(classes))
  xgc = get_xs_cpt_entries(x_cpts, cpt_inds, classes, N)
  cp <- params(x)[[class]]
  log_multiply_cp_xgc(xgc, cp, N)
}
#' Maps observations of x to cpt indices. Replicates the indices to cover each
#' class.
#' @return A list of integers. The cpt indices of each factor, repeated once for
#'   each class, and class indices.
#' @keywords internal
x2cpt_inds <- function(x_cpts, dataset, class, nclass) {
  if (length(x_cpts) == 0) return(list()) 
  features <- names(x_cpts)
  #   check cpt levels match levels in data set; that way I can use factor codes
  #   as indices.
  dataset <- dataset[ , features, drop = FALSE]
  data_levels <- extract_var_levels(dataset)
  # [features] omits the class and puts in same order as data levels
  var_values <- cpt_vars_values2(x_cpts)[features]
  if (!isTRUE(all.equal(data_levels, var_values, check.attributes = FALSE))) {
    stop("Levels in data set must match those in the CPTs (values(x)).")  
  }
  cpt_inds <- lapply(dataset, factor2cpt_inds, nclass)
  # Add class indices 
  cpt_inds[[class]] = rep(1:nclass, each = nrow(dataset))
  cpt_inds
}
# Maps an x factor to cpt indices. Makes a copy per each class
# @return An integer vector of length nclass * length(f)
factor2cpt_inds <- function(f, nclass) {
  stopifnot(is.factor(f), assertthat::is.count(nclass))
  # as.integer() is OK because it does not copy f
  rep(as.integer(f), times = nclass)
}
get_xs_cpt_entries <- function(x_cpts, inds, classes, N) {
  lapply(x_cpts, get_x_cpt_entries, inds, classes, N)
}
# Returns a list of cpt entries for the features. For each feature, it is an N x
# nclass matrix. These cpts do not necessarily correspond to a single dag; may
# be for multiple dags.
get_x_cpt_entries <- function(x_cpt, inds, classes, N) {
  nclass <- length(classes)
  stopifnot(assertthat::is.count(nclass))
  entries <- cpt_entries(x_cpt, inds)
  # One row per class.
  dim(entries) <- c(N, nclass)
  colnames(entries) <- classes
  entries
}
# Returns a vector of cpt entries.
# @param cpt_inds A list of indices, one per variable. Can contain more
#   variables than in cpt.
cpt_entries <- function(cpt, cpt_inds) {
	vars <- cpt2family(cpt)
	stopifnot(is_non_empty_complete(vars), is_subset(vars, names(cpt_inds)))	
	indices <- do.call('cbind', cpt_inds[vars])
	cpt[indices]	
}
# Computes the product (in log space) of P(C) and P(x | c) for N instances.
log_multiply_cp_xgc <- function(xgc, cp, N) {
  # Make cp have dimensions N x cp 
  mcp <- matrix(rep(cp, each = N), ncol = length(cp), 
                dimnames = list(NULL, names(cp)))
  factors <- xgc
  factors$class = mcp
#   Pass everything to log space 
#   Sum by Reduce. Here it is better if  I have matrices for conformity. 
  sum_log_factors(factors)
}
# Returns the variables in the cpts and their possible values. 
# Because tapply returns an array (of mode list), this also returns an array.
cpt_vars_values2 <- function(cpts) {
  d <- lapply(cpts, dimnames)
  # Get variables names. 
  vars <- unlist(lapply(d, names), use.names = FALSE) 
  # Get corresponding domains 
  values <- unlist(d, recursive = FALSE, use.names = FALSE)
  # For each, get the combinations . Keep as list. 
  var_values <- tapply(values, vars, single_unique_in_list, simplify = FALSE)
#   var_values <- lapply(var_values, unique)
#   #   Make sure variable domains are identical across CPTs
#   stopifnot(all(vapply(var_values, length, FUN.VALUE = integer(1)) == 1))
  # return in same order as cpts
  var_values[names(cpts)]
}
single_unique_in_list <- function(x) {
    stopifnot(is.list(x))
    u <- unique(x)
    # Must be consistent across CPTs
    stopifnot(length(u) == 1)
    unlist(u)
}
# # Returns a list of CPT entries per feature. Each features CPT entries are a matrix
# # of N rows and a column per class.
get_ccx_factors <- function(cptsx, dataset, class, classes) {
  # Return empty list if no cpts
  if (length(cptsx) == 0) return( list() )
  # Get variables (1D of cpts)
  features <- vapply(cptsx, cpt_1d_var, FUN.VALUE = character(1))
  #   Get x CPT indices replicated for each class
  # check_dataset_levels(cptsx, dataset)
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
# # Make a copy of class posterior per data point
make_cp_factor <- function(cp, n) {
  matrix(rep(cp, each = n), ncol = length(cp), dimnames = list(NULL, names(cp)))
}
# # Makes the indices to get values for observations of CPT variables and each
# # possible class
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
# Check that levels in dataset are identical to those in the CPTs. Need this to replace the factor values with their integer codes.
# 
# @param cptsx The feature cpts
# @keywords internal 
# check_dataset_levels <- function(cptsx, dataset) {  
#   # One variable appears in many CPTs. Are levels consistent among them?
#   names(cptsx) <- vapply(cptsx, cpt_1d_var, FUN.VALUE = character(1))
#   # Get just one CPT per feature
#   features <- unique(names(cptsx))
#   ucptsx <- cptsx[features]
#   cpt_levels <- lapply(cptsx, cpt_1d_values) 
#   # Get feature levels in dataset
#   dataset <- trim_dataset(features, dataset)
#   levels <- extract_var_levels(dataset)
#   if (!identical(cpt_levels, levels)) {
#     stop("The feature levels in data set must match those of values()")  
#   }
# }