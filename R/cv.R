#' Stratified cross validation estimate of predictive accuracy.
#' 
#' It may relearn only the parameters on each set of training folds, or relearn 
#' the structure, too. The options used to relearn parameters and, optionally,
#' structure, are those used to learn the objects in \code{x}.
#' 
#' @param x List of \code{\link{bnc_bn_object}}. The classifiers to evaluate.
#' @param dataset The data frame on which to evaluate the classifiers.
#' @param k An integer. The number of folds.
#' @param dag A logical. Whether to learn structure on each training set. 
#'   Parameters are always learned.
#' @param means A logical. Whether to return mean accuracy for each classifier 
#'   or to return a k-row matrix with accuracies per fold.
#' @inheritParams learn_params
#' @export
#' @return A numeric vector. The predictive accuracy of each classifier in 
#'   \code{x}. If \code{means = TRUE} then a matrix with k rows.
cv <- function(x, dataset, k, dag, means = TRUE) {
  xs <- ensure_multi_list(x, type = "bnc_bn")
  class <- get_common_class(xs)
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  test_folds <- partition_dataset(dataset, class, k)
  train <- lapply(test_folds, function(x) dataset[-x, , drop = FALSE])
  test <- lapply(test_folds, function(x) dataset[x, , drop = FALSE])
  p <- mapply(update_assess_fold, train, test, 
              MoreArgs = list(x = xs, dag = dag, class = class), SIMPLIFY = TRUE)
  p <- format_cv_output(p, ensure_list(x))
  if (means) {
    p <- colMeans(p)  
  }
  p
}
format_cv_output <- function(p, x) {
  if (is.null(dim(p)) || length(dim(p)) < 2) {
    stopifnot(length(x) == 1)
    # convert to a matrix 
    p <- t(p)
  }
  p <- t(p)
  stopifnot(ncol(p) == length(x))
  colnames(p) <- names(x)
  p
}
# Do a cross validation instance with the cross val package
update_assess_fold <- function(train, test, x, dag, class) {
  ux <- lapply(x, update, dataset = train, dag = dag)
  # Predict for each x 
  predictions <- lapply(ux, predict, test,  prob = FALSE)
  # Compute accuracy
  vapply(predictions, accuracy, test[[class]], FUN.VALUE = numeric(1))
}
# This works for a single partition.
cv_lp_partition <- function(x, train, test) {
  ux <- ensure_multi_list(x)
  stopifnot(is_just(train, "list"), is_just(test, "list"), 
            length(train) > 1, length(train) == length(test))
  p <- mapply(learn_and_assess, train, test, MoreArgs = list(x = ux), 
              SIMPLIFY = TRUE)
  p <- format_cv_output(p, ensure_list(x))
  colMeans(p)
}
learn_and_assess <- function(mem_cpts, test, x) {
  x <- ensure_multi_list(x)
  class <- get_common_class(x)
  x <- lapply(x, lp_implement, .mem_cpts = mem_cpts)
  predictions <- multi_predict(x, test,  prob = FALSE)
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
  if (n == 0) return(integer())
  stopifnot(n >= 1 && k > 1)
  # shuffle the folds, to try to fill them equally when k is not a multiple of n
  fold_ids <- sample(seq_len(k), k)
  fold_nums <- rep(fold_ids, length.out = n)
  # shuffle 
  sample(fold_nums, n)
}