# Computes class posterior. 
# x is a bnc_dag.
compute_cp <- function(x, dataset) {
  p <- compute_log_joint(x, dataset)
  p <- log_normalize(p)
  stopifnot(are_pdists(p))
  p
}
select_probs <- function(x, dataset, log_prob_pred_class) {
  # Find the entries in log prob corresponding to observed classes
  observed_classes <- as.character(dataset[, class_var(x)])
  class_probs <- subset_by_colnames(observed_classes, log_prob_pred_class)   
  class_probs 
}
#' Computes log-likelihood of the model on the provided data.
#' 
#' @param x A \code{\link{bnc_bn}} object.
#' @param dataset A data frame.
#' @keywords internal
compute_ll <- function(x, dataset) {
  # Get log joint prob per class 
  log_joint_per_class <- compute_log_joint(x, dataset)
  class_probs <- select_probs(x, dataset, log_joint_per_class) 
  # Sum them up 
  sum(class_probs)
}

#' Computes the conditional log-likelihood of the model on the provided data.
#' @keywords internal
compute_cll <- function(x, dataset) { 
    pred <- compute_cp(x = x, dataset = dataset)   
    pred <- log(pred) 
    class_probs <- select_probs(x, dataset, pred)  
    sum(class_probs)
}  
# Computes the log joint probability of the observed features for each of the classes
# The result is a numeric matrix with a column per class and a row per data instance.
compute_log_joint <- function(x, dataset) {
  if (!anyNA(dataset)) {
    compute_log_joint_complete(x, dataset)
  }
  else { 
    ind_complete <- complete.cases(dataset)
    p_complete <- 
      compute_log_joint_complete(x, dataset[ind_complete, , drop = FALSE])
    p_incomplete <- 
      compute_log_joint_incomplete(x, dataset[!ind_complete, , drop = FALSE])
    # put the two together
    p <- matrix(numeric(), nrow = nrow(dataset), ncol = ncol(p_incomplete), 
                   dimnames = list(NULL, dimnames(p_incomplete)[[2]]))
    p[ind_complete, ] <- p_complete
    p[!ind_complete, ] <- p_incomplete
    p
  }
} 
compute_log_joint_complete <- function(x, dataset) {
  UseMethod("compute_log_joint_complete")
}
compute_log_joint_complete.bnc_aode <- function(x, dataset) { 
  p <- lapply(x$models, compute_anb_log_joint_per_class, dataset = dataset)  
  p <- lapply(p, exp)
  # need to take the average!! 
  p <- Reduce('+', p)
  log(p)
}
compute_log_joint_complete.bnc_bn <- function(x, dataset) {
  compute_anb_log_joint_per_class(x, dataset)
}
# TODO: below
compute_log_joint_incomplete <- function(x, dataset) {
  # Check all rows in dataset have missings 
  stopifnot(sum(complete.cases(dataset)) == 0)
  # Check x is a bnc_bn
  check_bnc_bn(x)
  # Get gRain from bnc_bn 
  grain <- as_grain(x)
  # Get class var
  class <- class_var(x)
  # Call grain class posterior
  compute_grain_log_joint(grain, dataset, class)
}