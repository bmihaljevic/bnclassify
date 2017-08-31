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
#  Compute WANBIA
# 
#  How many max iterations?
#  MSE or CLL
# 
#  Numerically OK? 
# debug evolution of values of w
# 
# function of w. a point is a vector of weights 
# 0 and 1 are bounds for weights 
# initial w = 1 
compute_wanbia_weights <- function(class, dataset) {
  # stopifnot(is_nb(x))
  features <- setdiff(colnames(dataset), class)
  w <- rep(1, length(features))
  cll <- make_cll(class, dataset) 
  cll_gradient <- make_cll_gradient(class, dataset) 
  w <- optim(w, cll, cll_gradient, method = 'L-BFGS-B', lower = 0, upper = 1)
  setNames(w$par, features) 
}   