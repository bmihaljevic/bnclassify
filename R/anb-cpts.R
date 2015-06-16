# Checks cpts ordered according to vars() and 1D names correspond to vars()
cpts2families <- function(cpts) {
  lapply(cpts, cpt2family) 
}
check_anb_cpts <- function(cpts, class) {
  # Check families
  fams <- cpts2families(cpts)
  check_anb_families(fams, class)
  # Not checking the actual values in the CPTs...
}
# check_anb_cpt <- function() {
#   # check the family. that is, get the variables and that. 
# }
families2cpts <- function(families, dataset, smooth) {
  # Check dataset 
  check_dataset(dataset)
  lapply(families, extract_cpt, dataset, smooth = smooth)
}
extract_cpt <- function(vars, dataset, smooth) {
  ctgt2cpt(extract_ctgt(vars, dataset), smooth = smooth)
}
#' Turns a contingency table into a conditional probability table  
ctgt2cpt <- function(ctgt, smooth) {
  #   Check smooth is numeric and non-negative
  stopifnot(smooth >= 0)
  # Check ctgt is a table. That implies it is an array.
  stopifnot(is.table(ctgt))
  # Check it has got non-emtpy and no-NA dimnames 
  stopifnot(are_complete_dimnames(ctgt))
  #   Add smooth to ctgt 
  ctgt <- ctgt + smooth
  # Initialize cpt
  cpt <- NULL
  # If ctgt is 1D then cpt is normalized ctgt
  dnames <- dimnames(ctgt)
  if (length(dnames) == 1) {
    cpt <- normalize(ctgt)
  }
  else {
    #   Get the indices of conditioning variables
    conditioning <- setdiff(seq_along(dnames), 1)
    #   Condition the first variable on the rest. If some are NA, set to uniform
    cpt <- apply(ctgt, conditioning, normalize)
  }
  # Make sure it is an array
  cpt <- as.table(cpt)
  # Check dimnames
  stopifnot(identical(dimnames(cpt), dimnames(ctgt)))
  # Return 
  cpt
}
#' Get just form first dimension in their own cpt, not checking for consistency
#' in others.
#' @keywords internal
cpt_vars_values <- function(cpts) {
  # Check the names of cpts are equal to the name of their first dim 
  vars <- vapply(cpts, cpt_1d_var, FUN.VALUE = character(1))
  stopifnot(identical(unname(vars), names(cpts)))
  # Return the values
  lapply(cpts, cpt_1d_values)
}
#' Returns the name of the first dimensions and the values in the dimension of
#' the table.
cpt_1d_values <- function(cpt) {
  # Get 1d cases and check not empty
  values <- get_cpt_values(cpt)[[1]]
  check_non_empty_complete(values)
  values
}
cpt_1d_var <- function(cpt) {
  var <- cpt2family(cpt)[[1]]
  stopifnot(assertthat::is.string(var))
  var
}
cpt2family <- function(cpt) {
  # Check is a table
  stopifnot(is.table(cpt))
  # TODO: check is a CPT
  # Return names dimnames
  names(dimnames(cpt))
}
get_cpt_values <- function(cpt) {
  stopifnot(is.table(cpt))
  dimnames(cpt)
}
# Gets cpt entries using a list of indices
# Returns a vector
subset_cpt <- function(cpt, indices) {
  # check var non empty, in indices
  vars <- names(dimnames(cpt))
  stopifnot(is_non_empty_complete(vars), is_subset(vars, names(indices)))
  # Get index matrix from df 
  x_indices <- do.call('cbind', indices[vars])
  cpt[x_indices]
}