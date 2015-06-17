#' Makes sure that last is the last element in x 
make_last <- function(x, last) {
#   Check x and last are characters, last is length 1
  stopifnot(is.character(x), is.character(last), length(last) == 1)
#   Get all elements in x different than last
  non_last <- Filter(function(elem) { elem != last }, x)
#  Make sure there are n-1 such elements (i.e., last appears once in x) 
  stopifnot(length(non_last) + 1L == length(x))  
#   Append last to non_last
  append(non_last, last)  
}
#' Return all but last element of x. 
#' 
#' If x is NULL returns NA not NULL
get_but_last <- function(x) { 
  get_null_safe(x, -length(x))
} 
#' Return last element of x. 
#' 
#' If x is NULL returns NA not NULL
get_last <- function(x) { 
  get_null_safe(x, length(x))
} 
#' Get i-th element of x. 
#' 
#' If x is NULL returns NA not NULL
get_null_safe <- function(x, i) {
  if (length(x) == 0) NA else x[i]
}
# Convert a factor to integer and then replicate reps times
rep_factor_as_int <- function(f, reps) { 
  attributes(f) <- NULL
  stopifnot(is.integer(f))
  rep(f, reps)
}
#' Compares all elements in a to b
#' 
#' @param b numeric. Must be lenght one but no check is performed.
#' @keywords internal
fast_equal <- function(a, b) {
  #   stopifnot(length(b) == 1) No check for efficiency
  abs(a - b) < .Machine$double.eps ^ 0.5
}
accuracy <- function(x, y) {
  stopifnot(length(x) == length(y))
  sum(x == y) / length(x)
}
#  a list to a matrix where the names are kept in the second column
unlist_keepnames <- function(list) {
  lengths <- vapply(list, length, FUN.VALUE = integer(1))
  unname(cbind(unlist(list, use.names = FALSE), rep(names(list), lengths)))
}