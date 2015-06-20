#' @export
bnc <- function(dag_learner, class, dataset, smooth) {
  # It is easier to handle a funct. name than a funct. object in save_bnc_call
  stopifnot(assertthat::is.string(dag_learner))
  dag <- do.call(dag_learner, list(class = class, dataset = dataset))
  lp(dag, dataset = dataset, smooth = smooth)
}
#' Learn parameters.
#' @export
lp <- function(x, dataset, smooth) {
  bn <- bnc_bn(x, dataset, smooth)
  add_params_call_arg(bn, call = match.call(), env = parent.frame(), force = TRUE)
}
#' @export
#' @seealso \link{awnb}
lpawnb <- function(x, dataset, smooth, trees, bootstrap_size) {
  bn <- bnc_bn(x, dataset, smooth)
  weights <- awnb(class_var(bn), dataset = dataset, trees = trees, 
                  bootstrap_size = bootstrap_size)
  bn <- set_weights(bn, weights)
  add_params_call_arg(bn, call = match.call(), env = parent.frame(), force = TRUE)
}
set_weights <- function(bn, weights) {
  # Currently only expecting weights that can decrease the importance of a feature
  stopifnot(is_non_empty_complete(weights))
  stopifnot(all(weights >= 0 & weights <= 1))
  # Check weights correspond to features
  feats <- names(weights)
  stopifnot(is_non_empty_complete(feats), all(feats %in% features(bn)))
  # modify cpts directly
  bn$.params[feats] <- 
    mapply(exponentiate_cpt, bn$.params[feats], weights, SIMPLIFY = FALSE)
  # register weights
  bn$.weights <- weights
  bn
}