#' Learn a structure and parameters.
#' 
#' A convenience function to learn the structure and parameters in a single
#' call.
#' 
#' @inheritParams nb
#' @inheritParams learn_params
#' @param dag_learner A character. Name of the structure learning function.
#' @param dag_args A list. Optional additional arguments to \code{dag_learner}.
#' @param dataset The data frame from which to learn network structure and
#'   parameters.
#' @export
bnc <- function(dag_learner, class, dataset, smooth, dag_args = NULL) {
  # It is easier to handle a funct. name than a funct. object in save_bnc_call
  stopifnot(assertthat::is.string(dag_learner))
  dag_args <- append(list(class = class, dataset = dataset), dag_args)
  dag <- do.call(dag_learner, dag_args)
  lp(dag, dataset = dataset, smooth = smooth)
}
#' @export
#' @rdname learn_params
lp <- function(x, dataset, smooth) {
  bn <- bnc_bn(x, dataset, smooth)
  add_params_call_arg(bn, call = match.call(), env = parent.frame(), force = TRUE)
}
#' @export
#' @rdname learn_params
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