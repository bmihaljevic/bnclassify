# Adds a list of matrices. 
# 
# @return A numeric matrix. Multiplied in log space.
sum_matrices <- function(matrices) {
  # Must have at least on member 
  stopifnot(length(matrices) > 0)
  # Check all are numeric matrices of same size with same colnames
  n <- nrow(matrices[[1]])
  nobs <- ncol(matrices[[1]])
  values <- colnames(matrices[[1]])
  valid <- vapply(matrices, valid_factor, n, nobs, values, 
                  FUN.VALUE = logical(1L))
  stopifnot(all(valid))
  sum <- Reduce('+', matrices)
  stopifnot(identical(colnames(sum), values))
  sum
}
valid_factor <- function(x, nrow, ncol, colnames) {
  identical(dim(x), c(nrow, ncol)) && is.numeric(x) && identical(colnames, colnames(x))
}
# Normalizes a vector. If division by the sum is Nan then returns a uniform distribution. 
# Not checking the input for NAs for speed. Caller must do that. 
normalize <- function(x) {
  n <- x / sum(x)
  # check the quotient is NaN since it is unclear which tolerance to use if checking
  # sum(x) == 0
  if (is.nan(n[1])) {
    stopifnot(all(is.nan(n)))
    # keeps the dimnames of n 
    n[] <- 1 / length(x)
  }
  n 
}
#' Normalize log probabilities. 
#' 
#' Uses the log-sum-exp trick.
#' 
#' @references Murphy KP (2012). \emph{Machine learning: a probabilistic
#'   perspective}. The MIT Press. pp. 86-87.
#' @keywords internal
log_normalize <- function(lp) {
  stopifnot(is.matrix(lp))
  # Check p is matrix of log probs?(<= 0)
  # Normalize with log sum exp 
  log_probs <- lp - matrixStats::rowLogSumExps(lp)
  # exponentiate
  p <- exp(log_probs)
  # rowAnys Does not distinguish between NA and NaN. 
  nans <- which(matrixStats::rowAnys(p, value = NaN))
  if (length(nans) > 0) {
    p[nans, ] <- 1 / ncol(p)
  }
  # Assign a uniform where the result is NaN
#   ind_zero <- is.nan(p)
#   if (any(ind_zero)) {
#     p[ind_zero,] <- 1 / ncol(p)
#   }
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