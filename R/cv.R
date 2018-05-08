#' Estimate predictive accuracy with stratified cross validation.
#' 
#' Estimate predictive accuracy of a classifier with stratified cross 
#' validation. It learns the models from the training subsamples by repeating 
#' the learning procedures used to obtain \code{x}. It can keep the network 
#' structure fixed and re-learn only the parameters, or re-learn both structure 
#' and parameters.
#' 
#' @param x List of \code{\link{bnc_bn}} or a single 
#'   \code{\link{bnc_bn}}. The classifiers to evaluate.
#' @param dataset The data frame on which to evaluate the classifiers.
#' @param k An integer. The number of folds.
#' @param dag A logical. Whether to learn structure on each training subsample. 
#'   Parameters are always learned.
#' @param mean A logical. Whether to return mean accuracy for each classifier or
#'   to return a k-row matrix with accuracies per fold.
#' @inheritParams learn_params
#' @export
#' @return A numeric vector of same length as \code{x}, giving the predictive
#'   accuracy of each classifier. If \code{mean = FALSE} then a matrix with k
#'   rows and a column per each classifier in \code{x}.
#'   
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1) 
#' # CV a single classifier
#' cv(nb, car, k = 10) 
#' nb_manb <- bnc('nb', 'class', car, smooth = 1, manb_prior = 0.5) 
#' cv(list(nb=nb, manb=nb_manb), car, k = 10)
#' # Get accuracies on each fold
#' cv(list(nb=nb, manb=nb_manb), car, k = 10, mean = FALSE)
#' ode <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(score = 'aic')) 
#' # keep structure fixed accross training subsamples
#' cv(ode, car, k = 10, dag = FALSE)
cv <- function(x, dataset, k, dag = TRUE, mean = TRUE) {
  xs <- ensure_multi_list(x, type = "bnc_fit")
  class <- get_common_class(xs)
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  test_folds <- partition_dataset(dataset, class, k)
  train <- lapply(test_folds, function(x) dataset[-x, , drop = FALSE])
  test <- lapply(test_folds, function(x) dataset[x, , drop = FALSE])
  p <- mapply(update_assess_fold, train, test, 
              MoreArgs = list(x = xs, dag = dag, class = class), SIMPLIFY = TRUE)
  p <- format_cv_output(p, ensure_list(x))
  if (mean) {
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
  # predictions <- multi_predict(x, test,  prob = FALSE)
  predictions <- lapply(x, predict, test,  prob = FALSE, normalize  = FALSE)
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
