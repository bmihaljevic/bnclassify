#' @export
#' @rdname loglik 
AIC.bnc_bn <- function(object, ...) {
  ll <- logLik(object, ...)
  penalize_loglik(ll, k = 1)
}
#' @export
#' @rdname loglik 
BIC.bnc_bn <- function(object, ...) {
  ll <- logLik(object, ...)
  penalize_loglik(ll, k = log(nobs(ll)) / 2)
}
#' @export
#' @rdname loglik 
logLik.bnc_bn <- function(object, ...) {  
  dataset <- list(...)[[1]]
  if (is.null(dataset) || nrow(dataset) == 0) stop("Must provide data instances.")
  loglik <- compute_ll(x = object, dataset = dataset)  
  attr(loglik, "nobs") <- nrow(dataset)
  if (inherits(object, "bnc_bn")) attr(loglik, "df") <- nparams(object)
  class(loglik) <- "logLik"
  loglik
} 
#' @export
#' @rdname loglik 
cLogLik <- function(object, ...) {   
  dataset <- list(...)[[1]]
  compute_cll(object, dataset)
} 
penalize_loglik <- function(ll, k) {
  as.numeric(ll) - k * attr(ll, "df")
}
#' @export 
#' @describeIn inspect_bnc_bn Returns the number of free parameters in the model.
nparams <- function(x) {      
  sum(vapply(params(x), count_cpt_free_params, FUN.VALUE = numeric(1)))
}
#' @export
#' @describeIn inspect_bnc_bn Returns the posterior of each arc from the class
#'   according to the MANB method.
manb_arc_posterior <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (!is.null(x$.manb)) {
    return(x$.manb)
  }
  warning("MANB arc posterior probabilities have not been computed for x.")
  NULL
}
#' @export 
#' @describeIn inspect_bnc_bn Returns the AWNB feature weights.
awnb_weights <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (!is.null(x$.weights)) {
    return(x$.weights)
  }
  warning("AWNB weights have not been computed for x.")
  NULL
}