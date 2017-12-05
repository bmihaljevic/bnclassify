#' Returns a function to compute negative conditional log-likelihood given feature weights
#' @keywords internal
make_cll <- function(class_var, dataset) {   
  check_class_in_dataset(class_var, dataset)
  function(w) {  
    nb <- make_weighted_nb(w, class_var, dataset)   
    - compute_cll(nb, dataset) 
  }    
}
fit_nb <- function(class_var, dataset) { 
  lp(nb(class_var, dataset), dataset, smooth = 1)
} 
make_weighted_nb <- function(w, class_var, dataset) { 
  nb <- fit_nb(class_var, dataset) 
  nb <- set_weights(nb, setNames(w, features(nb) ))  
  nb 
}
#' Returns a function to compute the gradient of negative conditional log-likelihood with respect to feature weights
#' @keywords internal
make_cll_gradient <- function(class_var, dataset) { 
 check_class_in_dataset(class_var, dataset)
 function(w) {  
  weighted <- make_weighted_nb(w, class_var, dataset)  
  unweighted <- fit_nb(class_var, dataset) 
  compute_cll_gradients(weighted, unweighted, dataset)  
 } 
}  
compute_cll_gradients <- function(weighted, unweighted, dataset) {
  features <- features(weighted)
  stopifnot(identical(features, features(unweighted)))
  db <- lapply(dataset, as.character)
  feats <- db[features]
  params <- params(unweighted)[features]
  cp <- compute_cp(weighted, dataset = dataset)
  class_var <- class_var(weighted)
  grad <- mapply(cll_gradient_var, feats, params , MoreArgs = list(class = db[[class_var]], class_posterior = cp )) 
  grad 
}
cll_gradient_var <- function(x, cpt, class, class_posterior) { 
  stopifnot(is.character(x))
  log_theta <- log(cpt[x, ])
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
#' @return a named numeric vector
compute_wanbia_weights <- function(class, dataset) {
  features <- setdiff(colnames(dataset), class)
  # Initial weights are 1.
  w <- rep(1, length(features))
  cll <- make_cll(class, dataset) 
  cll_gradient <- make_cll_gradient(class, dataset) 
  w <- optim(w, cll, cll_gradient, method = 'L-BFGS-B', lower = 0, upper = 1)
  setNames(w$par, features) 
}   