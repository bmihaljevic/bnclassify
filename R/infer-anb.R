# Computes the log joint probability of the observed features for each of the classes.
#  This assumes that x is an augmented naive Bayes and that data is complete.
compute_anb_log_joint <- function(x, dataset) {
# TODO: check for aug nb
  stopifnot(!anyNA(dataset))
  cp <- params(x)[[class_var(x)]]
  wcp <- make_cp_factor(cp, dataset) 
#  Add class posterior to factors list
  factors <- list(class = wcp)
# Add the features' factors, if any 
  features <- features(x)
  if (length(features) > 0) {
    cptsx <- params(x)[features]
    xp <- get_ccx_factors(cptsx, dataset, class_var(x), names(cp))
    factors <- append(factors, xp)
  }
# Sum factors in log space
  sum_log_factors(factors)
}
# Returns a list of CPT entries per feature. Each features CPT entries are a matrix
# of N rows and a column per class.
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