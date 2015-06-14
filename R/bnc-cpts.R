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
#' just in their own cpt 1d, not checking in others. 
cpt_vars_values <- function(cpts) {
  # Get 1D names and values for each cpt 
  vars_values <- lapply(cpts, cpt_1d_values)
  # Check the names of cpts are equal to the name of their first dim 
  vars <- vapply(vars_values, function(v) v$var, FUN.VALUE = character(1))
  stopifnot(identical(unname(vars), names(cpts)))
  # Return the values
  lapply(vars_values, function(v) v$values)
}
#' Returns the name of the first dimensions and the values in the dimension of
#' the table.
cpt_1d_values <- function(cpt) {
  # Check it is table
  stopifnot(is.table(cpt))
  # Get 1d name and check not empty
  var <- names(dimnames(cpt))[[1]]
  stopifnot(length(var) > 0)
  # Get 1d cases and check not empty
  values <- dimnames(cpt)[[1]]
  check_non_empty_complete(values)
  # Return var name and values
  list(var = var, values = values)
}
get_cpt_vars <- function(cpt) {
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
get_cpts_vars <- function(cpts) {
  unique(unlist(lapply(cpts, get_cpt_vars), use.names = FALSE))
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