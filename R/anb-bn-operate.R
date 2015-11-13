#' Compute penalized log-likelihood of the Bayesian network classifier.
#' 
#' The Akaike information criterion (AIC) score:
#' 
#' \eqn{log P(\mathcal{D} \mid \theta) - \frac{1}{2} |\theta|},
#' 
#' where \eqn{|\theta|} is the number of free parameters.
#' This function must be passed a data frame argument - does not matter how it 
#' is named - in addition to the model (\code{object}). \code{object} must have
#' its structure and parameters specified.
#' 
#' @param object A \code{bnc_bn} object. 
#' @param ... A data frame. 
#' @export
AIC.bnc_bn <- function(object, ...) {
  ll <- logLik(object, ...)
  penalize_loglik(ll, k = 1)
}
#' Compute penalized log-likelihood of the Bayesian network classifier.
#' 
#' The Bayesian information criterion (BIC) score:
#' 
#' \eqn{log P(\mathcal{D} \mid \theta) - \frac{\log N}{2} |\theta|},
#' 
#' where \eqn{|\theta|} is the number of free parameters and N is the number of 
#' observations in the data set.
#' 
#' This function must be passed a data frame argument - does not matter how it 
#' is named - in addition to the model (\code{object}). \code{object} must have
#' its structure and parameters specified.
#' 
#' @param object A \code{bnc_bn} object.
#' @param ... A data frame.  
#' @export
BIC.bnc_bn <- function(object, ...) {
  ll <- logLik(object, ...)
  penalize_loglik(ll, k = log(nobs(ll)) / 2)
}
#' Compute log-likelihood of the Bayesian network classifier.
#' 
#' This function must be passed a data frame argument - does not matter how it 
#' is named - in addition to the model (\code{object}). \code{object} must have
#' its structure and parameters specified.
#' 
#' @param object A \code{bnc_bn} object.
#' @param ... A data frame. 
#' @seealso \code{\link{logLik}}.
#' @export
#' @examples
#' data(car)
#' nb.car <- lp(nb('class', car), car, smooth = 0)
#' logLik(nb.car, car)   
logLik.bnc_bn <- function(object, ...) {  
  dataset <- list(...)[[1]]
  if (is.null(dataset) || nrow(dataset) == 0) stop("Must provide data instances.")
  loglik <- compute_ll(x = object, dataset = dataset)  
  attr(loglik, "nobs") <- nrow(dataset)
  if (inherits(object, "bnc_bn")) attr(loglik, "df") <- nparams(object)
  class(loglik) <- "logLik"
  loglik
}
penalize_loglik <- function(ll, k) {
  as.numeric(ll) - k * attr(ll, "df")
}
#' @export 
#' @describeIn bnc_bn_object Returns the number of free parameters in the model.
nparams <- function(x) {      
  sum(vapply(params(x), count_cpt_free_params, FUN.VALUE = numeric(1)))
}

#' Returns the posterior of each arc from the class according to the MANB
#' method.
#'
#' The posterior must have already been computed for \code{x}. 
#' 
#' @export
#' @inheritParams bnc_bn_object
#' @references Dash D and Cooper GF (2002). Exact model averaging with naive
#'   Bayesian classifiers. \emph{19th International Conference on Machine
#'   Learning (ICML-2002)}, 91-98.
arc_posterior <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (!is.null(x$.manb)) {
    return(x$.manb)
  }
  warning("MANB arc posterior probabilities have not been computed for x.")
  NULL
}