skip_env <- new.env(parent = emptyenv()) 
skip_env$skip_assert <- FALSE
skip_env$skip_testing <- TRUE 
#' Whether to do checks or not. Set TRUE to speed up debugging or building. 
#' @keywords  internal
skip_assert <- function() {
  skip_env$skip_assert 
} 
#' Skip while testing to isolate errors
#' @keywords  internal
skip_testing <- function() {
  skip_env$skip_testing
}
# Tests whether two character vectors are identical when sorted
is_perm <- function(x, y) {
  is.character(x) && identical(sort(x), sort(y))
}
# use this e.g., in (I think) check_dataset
is_non_empty_complete <- function(x) {
  (length(x) > 0)  && (!anyNA(x))
}
# Compares two numerics ignoring attributes and class
equivalent_num <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))
  # Check.attributes = FALSE does not ignore class so remove it here
  class(x) <- NULL
  class(y) <- NULL
  all.equal(x, y, check.attributes = FALSE)
} 
is_subset <- function(x, y) {
  all(x %in% y)
}
is_positive <- function(x) {
  assertthat::is.number(x) && (x > 0)
}
is_nonnegative <- function(x) {
  assertthat::is.number(x) && (x >= 0)
}
is_last <- function(element, vector) { 
  # Basically does not work for numerics due to identical
  identical(element, get_last(vector))
}
is_at_pos <- function(element, position, vector) {
  # Basically does not work for numerics due to identical
  identical(get_null_safe(vector, position), element)
}
are_all_equal <- function(x) {
  length(unique(x)) == 1L
}
# use this e.g., in  check_dataset
are_all_unique <- function(x) {	
	length(x) == length(unique(x))
}
#' Checks if all columns in a data frame are factors.
#' 
#' @param x a \code{data.frame} 
#' @keywords internal
are_factors <- function(x) {
  #   If x is not data.frame stop
  stopifnot(is.data.frame(x))
  all(vapply(x, is.factor, FUN.VALUE = logical(1)))
}
is_just <- function(x, class) {
  identical(class(x), class)
}
## Checks
# Checks the object is non-empty and has no NA's
check_non_empty_complete <- function(x) {
  stopifnot(is_non_empty_complete(x))
}
are_complete_dimnames <- function(x) {
  if (skip_assert( )) return (TRUE)
  # Check x has non empty comlete dimames
  dnames <- dimnames(x)
  if (!is_non_empty_complete(dnames)) return (FALSE)
  # Check dimnames have non-empty complete names
  if (!is_non_empty_complete(names(dnames))) return (FALSE)
  # Check each dimension is non-empty complete
  all(vapply(dnames, is_non_empty_complete, FUN.VALUE = logical(1)))
}