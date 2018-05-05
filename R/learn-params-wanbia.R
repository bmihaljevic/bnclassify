# Using smooth = 1 by default
fit_wanbia_nb <- function(class_var, dataset) { 
  lp(nb(class_var, dataset), dataset, smooth = 1)
}  
#' Returns a function to compute negative conditional log-likelihood given feature weights
#' @keywords internal
make_cll <- function(class_var, dataset) {   
  check_class_in_dataset(class_var, dataset)
  function(w) {   
    nb <- fit_wanbia_nb(class_var, dataset)
    nb <- set_weights(nb, w)
    - compute_cll(nb, dataset) 
  }    
} 
#' Returns a function to compute the gradient of negative conditional log-likelihood with respect to feature weights
#' @keywords internal
make_cll_gradient <- function(class_var, dataset) { 
 check_class_in_dataset(class_var, dataset)
 function(w) {  
  unweighted <- fit_wanbia_nb(class_var, dataset)
  compute_cll_gradients(unweighted, dataset, w)  
 } 
}  
compute_cll_gradients <- function(unweighted, dataset, w) {
  stopifnot(is.null(unweighted$.weights))
  features <- features(unweighted)
  db <- lapply(dataset, as.character)
  feats <- db[features]
  params <- params(unweighted)[features]
  
  weighted <- set_weights(unweighted, w)
  cp <- compute_cp(weighted, dataset = dataset)
  class_var <- class_var(weighted)
  grad <- mapply(cll_gradient_var, feats, params , MoreArgs = list(class = db[[class_var]], class_posterior = cp )) 
  grad 
}
#' Assuming that the cpt is a leaf, returns 1 instead of a CPT entry when value missing
#' @param  x a vector of values
#' @keywords  internal
get_log_leaf_entries  <- function(cpt, x) { 
  stopifnot(is.character(x), length(dim(cpt)) == 2) 
  entries <- matrix(numeric(length = length(x) *  ncol(cpt)), ncol = ncol(cpt))
  colnames(entries) <- colnames(cpt)
  for (i in seq_along(x)) {
    value <- x[i]
    if (is.na(value)) { 
      entries[i, ] <- 1
    }
    else { 
      entries[i, ] <- cpt[value, ] 
    }
  }
  log(entries)
}
cll_gradient_var <- function(x, cpt, class, class_posterior) { 
  log_theta <- get_log_leaf_entries(cpt, x)  
  stopifnot(identical(dim(class_posterior), dim(log_theta )))
  log_theta_class <- subset_by_colnames(class, log_theta) 
  sum(- log_theta_class + rowSums(log_theta * class_posterior)) 
} 
#' Compute WANBIA weights.
#'  
#' Computes feature weights by optimizing conditional log-likelihood.  
#' Weights are bounded to [0, 1]. Implementation based on the original paper 
#' and the code provided at \url{http://sourceforge.net/projects/rawnaivebayes}.
#' 
#' @inheritParams nb
#' @param class character 
#' @param dataset The data frame from which to learn feature weights
#' @param return_optim_object Return full output of `optim`
#' @return a named numeric vector
#' @keywords internal
compute_wanbia_weights <- function(class, dataset, return_optim_object = FALSE) {
  features <- get_features(class = class, dataset = dataset)
  # Initial weights are 1.
  w <- rep(1, length(features))
  w <- setNames(w, features)
  cll <- make_cll(class, dataset) 
  cll_gradient <- make_cll_gradient(class, dataset) 
  w <- optim(w, cll, cll_gradient, method = 'L-BFGS-B', lower = 0, upper = 1)
  if (w$convergence != 0) warning(paste0("WANBIA did not converge correctly. ", w$message))
  if (return_optim_object ) return (w)
  w$par
}   