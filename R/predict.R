#' Predict. 
#' 
#' Ties are resolved randomly.
#' 
#' @export
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
#' @param x List of \code{\link{bnc_bn}}. The classifiers to evaluate.
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
multi_predict <- function(object, newdata, prob = FALSE) {
  #   if complete, then all one together
  if (!anyNA(newdata)) {
    p <- multi_compute_augnb_luccpx(object, newdata)
    p <- lapply(p, log_normalize)
    stopifnot(all(vapply(p, are_pdists, FUN.VALUE = logical(1))))
    if (prob) {
      p
    }
    else {
      lapply(p, map)
    }
  }
  #   otherwise get posterior for each separately
  else {
    lapply(object, predict, newdata,  prob = prob)
  }  
}