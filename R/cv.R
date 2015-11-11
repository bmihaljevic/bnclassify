#' Stratified cross validation estimate of predictive accuracy.
#' 
#' @param x List of \code{\link{bnc_bn_object}}. The classifiers to evaluate.
#' @param dataset The data frame on which to evaluate the classifiers.
#' @param k An integer. The number of folds.
#' @param dag A logical. Whether to learn structure on each training set.
#'   Parameters are always learned.
#' @inheritParams learn_params
#' @export
#' @return A numeric vector. The predictive accuracy of each classifier in 
#'   \code{x}.
cv <- function(x, dataset, k, dag, smooth = NULL) {
  multi_crossval(x, dataset = dataset, k = k, dag = dag, smooth = smooth)
}
# This works for a single partition.
cv_fixed_partition <- function(x, train, test, smooth) {
  # need > 1 because if result is 1 dimensional I assume that there is only one x 
  stopifnot(length(train) > 1, length(train) == length(test))
  p <- mapply(learn_and_assess, train, test, 
              MoreArgs = list(x = x, smooth = smooth), SIMPLIFY = TRUE)
  if (is.null(dim(p)) || length(dim(p)) < 2) {
    p <- t(p)
  }
  rowMeans(p)
}
learn_and_assess <- function(train, test, x, smooth) {
  x <- ensure_multi_list(x)
  class <- get_common_class(x)
  x <- lapply(x, lp, dataset = train, smooth = smooth)
  predictions <- lapply(x, predict, test,  prob = FALSE)
  vapply(predictions, accuracy, test[, class], FUN.VALUE = numeric(1))
}
partition_dataset <- function(dataset, class, k) { 
  check_class_in_dataset(class = class, dataset = dataset)
  make_stratified_folds(dataset[[class]], k = k)
}
make_stratified_folds <- function(class, k) {
  stopifnot(is.factor(class), k > 1, k <= length(class))
  grouped_inds <- split(seq(class), class)
  ns <- vapply(grouped_inds, length, FUN.VALUE = integer(1))
  fold_nums <- lapply(ns, distribute_class_over_folds, k)
  fold_nums <- unlist(fold_nums, use.names = FALSE)
  inds <- unlist(grouped_inds, use.names = FALSE)
  split(seq(inds)[inds], fold_nums)
}
# Distribute instances of a class over the k folds in approximately uniform
# fashion.
distribute_class_over_folds <- function(n, k) {
  stopifnot(n >= 1 && k > 1)
  # shuffle the folds, to try to fill them equally when k is not a multiple of n
  fold_ids <- sample(seq_len(k), k)
  fold_nums <- rep(fold_ids, length.out = n)
  # shuffle 
  sample(fold_nums, n)
}