#'Predicts class label or class posterior probability distributions.
#'
#'@details Ties are resolved randomly. Inference is much slower if
#'\code{newdata} contains \code{NA}s.
#'
#'@export
#'
#'@param object A \code{\link{bnc_bn_object}} object.
#'@param newdata A data frame containing observations whose class has to be 
#'  predicted.
#'@param prob A logical. Whether class posterior probability should be returned.
#'@param ... Ignored.
#'@return If \code{prob=FALSE}, then returns a length-\eqn{N} factor with the 
#'  same levels as the class variable in \code{x}, where \eqn{N} is the number 
#'  of rows in \code{newdata}. Each element of the factor is the most likely 
#'  class for the corresponding row in \code{newdata}. If \code{prob=TRUE}, 
#'  returns a \eqn{N} by \eqn{C} numeric matrix, where \eqn{C} is the number of 
#'  classes; each row corresponds to the class posterior of the instance.
predict.bnc_bn <- function(object, newdata, prob = FALSE, ...) {      
  # stopifnot(inherits(object, "bnc_fit"))  
  pred <- compute_cp(x = object, dataset = newdata)  
  if (!prob) {
    pred <- map(pred)
  }
  pred
}
#' Assigns instances to the most likely class.
#' 
#' Ties are resolved randomly.
#' 
#' @param pred A numeric matrix. Each row corresponds to class posterior 
#'   probabilities for an instance.
#' @param x a \code{bnc_fit} object.
#' @return a factor with the same levels as the class variable.
#' @keywords internal
map <- function(pred) {
  max_ind <- max.col(m = pred, ties.method = "random") 
  classes <- colnames(pred)
  stopifnot(is_non_empty_complete(classes))
  predicted <- classes[max_ind]
  #   Return a factor with the levels of the class variable
  factor(predicted, levels = classes)
}
#' Stratified cross validation estimate of predictive accuracy.
#' 
#' @param x List of \code{\link{bnc_bn_object}}. The classifiers to evaluate.
#' @param dataset The data frame on which to evaluate the classifiers.
#' @param k An integer. The number of folds.
#' @param dag A logical. Whether to repeat structure learning on each fold or 
#'   just parameter learning.
#' @inheritParams learn_params
#' @export
#' @return A numeric vector. The predictive accuracy of each classifier in 
#'   \code{x}.
cv <- function(x, dataset, k, dag, smooth = NULL) {
  multi_crossval(x, dataset = dataset, k = k, dag = dag, smooth = smooth)
}