#' Learn network structure and parameters.
#' 
#' A convenience function to learn the structure and parameters in a single 
#' call. Must provide the name of the structure learning algorithm function;
#' see \code{\link{bnclassify}} for the list.
#' 
#' @inheritParams nb
#' @inheritParams learn_params
#' @param dag_learner A character. Name of the structure learning function.
#' @param dag_args A list. Optional additional arguments to \code{dag_learner}.
#' @param dataset The data frame from which to learn network structure and 
#'   parameters.
#' @export
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' nb_manb <- bnc('nb', 'class', car, smooth = 1, manb_prior = 0.3)
#' ode_cl_aic <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(score = 'aic'))
bnc <- function(dag_learner, class, dataset, smooth, dag_args = NULL, 
                awnb_trees = NULL, awnb_bootstrap = NULL,
                manb_prior = NULL, wanbia = NULL) {
  # It is easier to handle a funct. name than a funct. object in save_bnc_call
  stopifnot(assertthat::is.string(dag_learner))
  dag_args <- append(list(class = class, dataset = dataset), dag_args)
  dag <- do.call(dag_learner, dag_args)
  lp(dag, dataset = dataset, smooth = smooth, awnb_trees = awnb_trees, 
     awnb_bootstrap = awnb_bootstrap, manb_prior = manb_prior, wanbia = wanbia)
}
#' @export
#' @rdname learn_params
lp <- function(x, dataset, smooth, awnb_trees = NULL, awnb_bootstrap = NULL,
               manb_prior = NULL, wanbia = NULL) {
  bn <- lp_implement(x = x, dataset = dataset, smooth = smooth, 
                     awnb_trees = awnb_trees, awnb_bootstrap = awnb_bootstrap,
                     manb_prior = manb_prior, wanbia = wanbia)
  # TODO: this should only somehow be in lp_implement
  # check_bnc_bn(bn) 
  add_params_call_arg(bn, call = match.call(), env = parent.frame(), force = TRUE)
}    
#' @keywords internal
lp_implement <- function(x, dataset, smooth, awnb_trees = NULL, 
                         awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL, .mem_cpts=NULL, ...) {
  UseMethod("lp_implement")
}  
#' @export
lp_implement.bnc_aode <- function(x, dataset, smooth, awnb_trees = NULL, 
                         awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL, .mem_cpts=NULL, ...) {
  models <- lapply(models(x), lp_implement, dataset = dataset, smooth = smooth) # TODO: pass mem_cpts, wanbia and other parameters to lp_implement?? 
  bnc_aode_bns(x, models) 
}    
#' @export
lp_implement.bnc_dag <- function(x, dataset, smooth, awnb_trees = NULL, 
                         awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL, .mem_cpts=NULL, ...) {
  params <- families2cpts(families(x), dataset = dataset, smooth = smooth,
                          .mem_cpts = .mem_cpts)
  bn <- make_bnc_bn(x, params = params)
  awnb <- (!is.null(awnb_trees) || !is.null(awnb_bootstrap))
  manb <- !is.null(manb_prior)
  wanbia <- !is.null(wanbia)
  if (sum(awnb + manb + wanbia) > 1) stop("Either MANB, AWNB, WANBIA can be applied, not more than one.")
  if (awnb) {
    weights <- awnb(class_var(bn), dataset = dataset, trees = awnb_trees, 
                    bootstrap_size = awnb_bootstrap)
    bn <- set_weights(bn, weights)  
  }
  if (manb) {
    ctgts <- lapply(families(x), extract_ctgt, dataset)[features(x)]
    arc_probs <- compute_manb_arc_posteriors(x, ctgts = ctgts, smooth = smooth,
                                             prior = manb_prior)
    bn <- include_manb_probs(bn, arc_probs, ctgts = ctgts, smooth = smooth)
  }
  if (wanbia) { 
    weights <- compute_wanbia_weights(class_var(bn), dataset = dataset)
    bn <- set_weights(bn, weights)  
  }
  bn
} 
set_weights <- function(bn, weights) {
  stopifnot(inherits(bn, "bnc_bn"))  
  # Currently only expecting weights that can decrease the importance of a feature
  stopifnot(is_non_empty_complete(weights))
  stopifnot(all(weights >= 0 & weights <= 1))
  # Check weights correspond to features
  feats <- names(weights)
  stopifnot(is_non_empty_complete(feats), identical(sort(feats), sort(features(bn))))
  # modify cpts directly
  bn$.params[feats] <- 
    mapply(exponentiate_cpt, bn$.params[feats], weights, SIMPLIFY = FALSE)
  # register weights. ensure they are in the same order as features(bn)
  bn$.weights <- weights[features(bn)]
  bn
}
include_manb_probs <- function(bn, arc_probs, ctgts, smooth) {
  feats <- names(arc_probs)
  stopifnot(inherits(bn, "bnc_bn"), identical(feats, features(bn)),
            identical(feats, names(ctgts)))  
  # modify cpts 
  bn$.params[feats] <- mapply(compute_manb_cpt, ctgts, arc_probs, 
                         MoreArgs = list(smooth = smooth), SIMPLIFY = FALSE)
  # register arc probs
  bn$.manb <- arc_probs
  bn
}