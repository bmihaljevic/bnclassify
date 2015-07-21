# Checks cpts ordered according to vars() and 1D names correspond to vars()
cpts2families <- function(cpts) {
  lapply(cpts, cpt2family) 
}
families2cpts <- function(families, dataset, smooth) {
  # Check dataset 
  check_dataset(dataset)
  lapply(families, extract_cpt, dataset, smooth = smooth)
}
extract_cpt <- function(vars, dataset, smooth) {
  ctgt <- extract_ctgt(vars, dataset)
  ctgt2cpt(ctgt, smooth = smooth)
}
# Turns a contingency table into a conditional probability table  
ctgt2cpt <- function(ctgt, smooth) {
  # Requiring ctgt be a table. That implies it is an array.
  stopifnot(smooth >= 0, is.table(ctgt), are_complete_dimnames(ctgt))
  # Add smooth to ctgt 
  ctgt <- ctgt + smooth
  normalize_ctgt(ctgt)
}
# Normalizes the contigency table on the first dimension. Returns a table.
normalize_ctgt <- function(ctgt) {
  stopifnot(!anyNA(ctgt))  # TODO: Should check for NaN
  # Keep attributes (e.g., class and dimension names); just change entries
  cpt <- ctgt
  ndims <- length(dim(ctgt))
  if (ndims == 1) {
    cpt[] <- normalize(cpt)
  }
  else if (ndims > 1) { 
    cpt[] <- apply(ctgt, 2:ndims, normalize)
  }
  else {
    stop("0 dimension of", ctgt)
  }
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
# Returns the name of the first dimensions and the values in the dimension of
# the table.
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
exponentiate_cpt <- function(cpt, value) {
  normalize_ctgt(cpt ^ value)
}
get_cpt_id <- function(cpt) {
  make_family_id(cpt2family(cpt))
}