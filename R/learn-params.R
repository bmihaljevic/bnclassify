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
lp <- function(x, dataset, smooth, awnb_trees = NULL, awnb_bootstrap = NULL) {
  bn <- lp_implement(x = x, dataset = dataset, smooth = smooth, 
                     awnb_trees = awnb_trees, awnb_bootstrap = awnb_bootstrap)
  check_bnc_bn(bn) 
  add_params_call_arg(bn, call = match.call(), env = parent.frame(), force = TRUE)
}
#' AWNB weights. 
#' 
#' Deprecated. Use \code{lp} instead.
#' 
#' @export
#' @inheritParams nb 
#' @inheritParams learn_params
#' @param trees An integer. The number (\eqn{M}) of bootstrap samples to 
#'   generate.
#' @param bootstrap_size A numeric. The size of the bootstrap subsample, 
#'   relative to the size of \code{dataset} (given in [0,1]).
lpawnb <- function(x, dataset, smooth, trees, bootstrap_size) {
  .Deprecated("lp")
  lp(x, dataset, smooth, awnb_trees = trees, awnb_bootstrap = bootstrap_size)  
}
lp_implement <- function(x, dataset, smooth, awnb_trees = NULL,
                         awnb_bootstrap = NULL, .mem_cpts = NULL) {
  params <- families2cpts(families(x), dataset = dataset, smooth = smooth,
                          .mem_cpts = .mem_cpts)
  bn <- make_bnc_bn(x, params = params)
  awnb <- (!is.null(awnb_trees) || !is.null(awnb_bootstrap))
  # TODO: if manb && awnb stop("Either MANB or AWNB")
  if (awnb) {
    weights <- awnb(class_var(bn), dataset = dataset, trees = awnb_trees, 
                    bootstrap_size = awnb_bootstrap)
    bn <- set_weights(bn, weights)  
  }
  bn
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