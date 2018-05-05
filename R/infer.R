# Computes class posterior. 
# x is a bnc_dag.
compute_cp <- function(x, dataset, normalize = TRUE) {
  p <- compute_log_joint(x, dataset)
  if (normalize ) {
    p <- log_normalize(p)
  } 
  p <- exponentiate_probs(p)   
  if (normalize ) {
    stopifnot(are_pdists(p))  
  }
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
  # TODO:  NA checks should be done at instance level in Rcpp,
  # as for the NB eg inference is possible even with incomplete data 
  dataset <- make_evidence(dataset, x) 
  if (!evidence_has_nas(dataset)) {
    compute_log_joint_complete(x, dataset)
  }
  else { 
    # TODO:  should filter complete according to the variables in the model.
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
#' @keywords internal
compute_log_joint_complete <- function(x, dataset) {
  UseMethod("compute_log_joint_complete")
} 
#' @export
compute_log_joint_complete.bnc_aode <- function(x, dataset) { 
  # TODO: validate aode: at least one model , or two models?
  stopifnot(nmodels(x) > 0)
  p <- lapply(models(x), compute_log_joint_complete, dataset = dataset)  
  average_aode(p)  
} 
#' @export
compute_log_joint_complete.bnc_bn <- function(x, dataset) {
  # compute_anb_log_joint_per_class(x, dataset)
  compute_joint(x, dataset)
}  
#' @keywords internal
compute_log_joint_incomplete <- function(x, dataset) {
  UseMethod("compute_log_joint_incomplete")
}  
#' @export
compute_log_joint_incomplete.bnc_aode <- function(x, dataset) {  
  # TODO: validate aode: at least one model , or two models? 
  stopifnot(nmodels(x) > 0)
  p <- lapply(models(x), compute_log_joint_incomplete, dataset = dataset)  
  average_aode(p)  
}
# take the average
# p is a list of matrices of log joint 
average_aode <- function(p) {
  stopifnot(is.list(p))
  nmodels <- length(p)
  p <- lapply(p, exp) 
  p <- Reduce('+', p)
  p <- p / nmodels 
  log(p)
} 
#' @export
compute_log_joint_incomplete.bnc_bn <- function(x, dataset) {
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
}#
# A decorated data frame.
new_evidence <- function(evidence,  hasna) {
 stopifnot(is.data.frame(evidence), is.logical(hasna))
 class(evidence) <- c('bnc_evidence', class(evidence)) 
 attr(evidence, "hasnas") <- hasna
 evidence
}
# Adds information to the data sets. Also potentially checks it.
# This could call Rcpp directly. 
# This is to avoid has_nas each time from greedy search
make_evidence <- function(dataset, x = NULL) {
  # TODO: this should go to the constructor of evidence, and be done there. As done currently, when calling compute_joint directly, all code in make_evidence is skipped.
  evidence <- dataset 
  if (!inherits(evidence, "bnc_evidence")) {   
    features <- NULL
    if (!is.null(x)) { 
      features <- features(x) 
      # check all features in dataset
      #  checks cpts match the dataset 
      # only for those that have params() defined; will not work for an aode. refactor this.
      dataset <- trim_dataset_cpp(dataset, features)
      if (inherits(x, 'bnc_bn')) { 
        data_levels <- lapply(dataset, levels)[features]
        #   # [features] omits the class and puts in same order as data levels
        var_values <- cpt_vars_values(params(x))[features]
        if (!isTRUE(all.equal(data_levels, var_values, check.attributes = FALSE))) {
          stop("Levels in data set must match those in the CPTs (values(x)).")  
        } 
      }
    }  
    hasna <- hasna_features(dataset, features )
    evidence <- new_evidence(evidence,  hasna)
  }
  # check evidence? 
       # has hasna. it is logical.
  evidence
}
evidence_has_nas <- function(evidence) {
 stopifnot(inherits(evidence, "bnc_evidence"))  
 attr(evidence, "hasnas") 
}