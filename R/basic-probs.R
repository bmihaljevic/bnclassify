#' Multiplies a set of factors by converting to log space and then summing. 
#' Subtracts the logarithm of the sum of exponential (logsumexp).
#' 
#' @return A numeric matrix. The values in log space.
#' @references Murphy KP (2012). \emph{Machine learning: a probabilistic
#'   perspective}. The MIT Press. pp. 86-87.
#' @keywords internal
sum_log_factors <- function(factors) {
  # Must have at least on member 
  stopifnot(length(factors) > 0)
  # Check all factors are numeric matrices of same size with same colnames
  n <- nrow(factors[[1]])
  nobs <- ncol(factors[[1]])
  values <- colnames(factors[[1]])
  valid <- vapply(factors, valid_factor, n, nobs, values, FUN.VALUE=logical(1L))
  stopifnot(all(valid))
#   Apply log to all
  factors <- lapply(factors, log)
#   Sum them 
  log_sum <- Reduce('+', factors)
  # Log sum exp 
  log_sum <- log_sum - matrixStats::rowLogSumExps(log_sum)
  colnames(log_sum) <- values
  log_sum
}
valid_factor <- function(x, nrow, ncol, colnames) {
  identical(dim(x), c(nrow, ncol)) && is.numeric(x) && identical(colnames, colnames(x))
}
# Normalizes a vector. If division by the sum is Nan then returns a uniform distribution.
normalize <- function(x) {
  stopifnot(!anyNA(x))
  n <- x / sum(x)
  if (all(is.nan(n))) {
    # keeps the dimnames of n 
    n[] <- 1/length(x)
  }
  n 
}
log_normalize <- function(ulp) {
  # Check p is matrix of log probs?(<= 0)
  # exponentiate
  p <- exp(ulp)
  # Normalize 
  sums <- rowSums(p)
  p <- p / sums
  # Assign a uniform where the sum is 0
  ind_zero <- is.nan(sums)
  if (any(ind_zero)) {
    p[ind_zero,] <- 1 / ncol(p)
  }
  p
}
#' Returns \code{TRUE} is \code{x} is a valid probability distribution.
#' 
#'@keywords internal
are_pdists <- function(x) {
  stopifnot(is.matrix(x))  
  are_probs(x) && all(fast_equal(rowSums(x), 1))  
}
are_probs <- function(x) {
  !anyNA(x) && all(x >= 0) && all(x <= 1)
}