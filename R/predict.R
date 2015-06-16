#' Predict. 
#' @export
predict.bnc_bn <- function(object, newdata, prob = F, ...) {      
  # stopifnot(inherits(object, "bnc_fit"))  
  pred <- compute_cp(x = object, dataset = newdata)  
  if (!prob) {
    pred <- map(pred, object)
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
map <- function(pred, x) {
  max_ind <- max.col(m = pred, ties.method = "random") 
  classes <- classes(x = x)
  predicted <- classes[max_ind]
  #   Return a factor with the levels of the class variable
  factor(predicted, levels = classes)
}
#' CV
#' @export
bnc_cv <- function(x, dataset, k, dag = FALSE) {
  # Ensure x is list because cross val expects one
  if (!inherits(x, "list")) {
    x <- list(x)
  }
  # TODO: Check that all x have the same class?
  class <- class_var(x[[1]])
  update_args <- lapply(x, bnc_get_update_args, dag = dag)
  do_crossval(update_args, class = class, dataset = dataset, k = k)
}
dag_cv <- function(x, lp_args, class, dataset, k) {
  updateable <- lapply(x, make_daglp_updateable, lp_args)
  update_args <- lapply(updateable, bnc_get_update_args, dag = FALSE)
  do_crossval(update_args, class = class, dataset = dataset, k = k)
}