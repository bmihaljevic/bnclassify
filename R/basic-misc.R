#' Return all but last element of x. 
#' 
#' If x is NULL returns NA not NULL
#' @keywords internal
get_but_last <- function(x) { 
  get_null_safe(x, -length(x))
} 
#' Return last element of x. 
#' 
#' If x is NULL returns NA not NULL
#' @keywords internal
get_last <- function(x) { 
  get_null_safe(x, length(x))
} 
#' Get i-th element of x. 
#' 
#' If x is NULL returns NA not NULL
#' @keywords internal
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
#' @param b numeric. Must be length one but no check is performed.
#' @keywords internal
fast_equal <- function(a, b) {
  #   stopifnot(length(b) == 1) No check for efficiency
  abs(a - b) < .Machine$double.eps ^ 0.5
}
#' Compute predictive accuracy. 
#' 
#' @param x A vector of predicted labels.
#' @param y A vector of true labels.
#' @export
#' 
#' @examples  
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' p <- predict(nb, car)
#' accuracy(p, car$class)
accuracy <- function(x, y) {
  count_equal(x, y) / length(x)
}
#  a list to a matrix where the names are kept in the second column
unlist_keepnames <- function(list) {
  lengths <- element_lengths(list)
  unname(cbind(unlist(list, use.names = FALSE), rep(names(list), lengths)))
}
element_lengths <- function(list) {
  vapply(list, length, FUN.VALUE = integer(1))
}
max_random <- function(x) {
  ind <- which(fast_equal(x, max(x)))
  if (length(ind) > 1) {
    ind <- sample(ind, 1)
  }
  ind  
}
#' Return a bootstrap sub-sample.
#' 
#' @param dataset a \code{data.frame}  
#' @param proportion numeric given as fraction of \code{dataset} size
#' @keywords internal
bootstrap_ss <- function(dataset, proportion) {
  stopifnot(is_positive(proportion))
  N <- nrow(dataset)
  stopifnot(N > 0)
  subsample_size <- trunc(N * proportion)
  dataset[sample(N, replace = T, size = subsample_size), , drop = FALSE]
}
make_call <- function(f, args) {
  f <- as.name(f)
  as.call(c(f, args))
}
#' Subset a 2D structure by a vector of column names.
#' 
#' Not all colnames are necessarily in the columns of data; in that case this
#' returns NA.
#' @param colnames a character vector
#' @param data a matrix or data frame 
#' @keywords internal
subset_by_colnames <- function(colnames, data) {
  stopifnot(is.character(colnames), length(colnames) == nrow(data)) 
  ind_cols <- match(colnames, colnames(data))
  ind_matrix <- cbind(seq_along(ind_cols), ind_cols)
  data[ind_matrix]   
}