make_cll <- function(class_var, dataset) {  
  function(w) { 
  nb <- lp(nb(class_var, dataset), dataset, smooth = 1)
  nb <- set_weights(nb, setNames(w, features(nb) )) 
  
  pred <- compute_cp(x = nb, dataset = dataset)  
    class <- as.character(dataset[[class_var]])
  - sum(log(mlearn::subset_by_colnames(class, pred ) ))
  }    
}
# gradient is a vector 
# ***is as.character ok? better way? **
make_cll_gradient <- function(class_var, dataset) {
 function(w) { 
  nb <- lp(nb(class_var, dataset), dataset, smooth = 1)
  nb <- set_weights(nb, setNames(w, features(nb) ))  
  cp <- compute_cp(nb, dataset = dataset)
  db <- lapply(dataset, as.character)
  feats <- db[features(nb)]
  params <- params(nb)[features(nb)]
  mapply(cll_gradient_var, feats, params , MoreArgs = list(class = db[[class_var]], cp = cp ))
 } 
} 
cll_gradient_var <- function(x, cpt, class, cp) { 
  # must mulptily by class, then it is OK
  stopifnot(nrow(cp) == length(x))
  theta <- cpt[x, ]
  theta_class <- mlearn::subset_by_colnames(class, theta) 
  sum(- log(theta_class) + diag(log(theta) %*% t(cp)))
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