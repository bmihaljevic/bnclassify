# Computes the 
compute_augnb_luccpx <- function(x, dataset) {
# TODO: check it is aug nb, only that way the factorization will work
#   Make a copy of class posterior per data point
  n <- nrow(dataset)
  cp <- bnc_params(x)[[bnc_class(x)]]
  nclass <- length(cp)
  wcp <- matrix(rep(cp, each=n), ncol=nclass, dimnames=list(NULL, names(cp)))
#  Add class posterior to factors list
  factors <- list(wcp)
# If there are features, get the class conditional probabilities
  features <- bnc_features(x)
  if (length(features) > 0) {
  #   Get x CPT indices replicated for each class
    # warning("check dataset levels not implemented.")  
    xcinds <- make_xcpt_indices(features, bnc_class(x), nclass, dataset)
  #   Get x CPT entries
    cptsx <- bnc_params(x)[features]
    xp <- lapply(cptsx, subset_cpt, xcinds)
    # Reshape entries to make a column per class
    for (i in seq_along(xp)) {
      dim(xp[[i]]) <- c(n, nclass)
      colnames(xp[[i]]) <- names(cp)
    }
    factors <- append(factors, xp)
  }
#   Multiply factors
  sum_log_factors(factors)
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