 #' Returns a \code{c("bnc_aode", "bnc")} object.
#' @keywords internal
bnc_aode <- function(models, class_var, features) {
  stopifnot(length(models) > 0, identical(names(models), unname(features)))
  stopifnot(all(vapply(models, is_ode, FUN.VALUE = logical(1))))
  bnc <- bnc_base(class = class_var, features = features)
  bnc$.models <- models
  class(bnc) <- c('bnc_aode', class(bnc))
  bnc
}
#' Fits an AODE model.
#' @keywords internal
bnc_aode_bns <- function(x, fit_models) {
  stopifnot(inherits(x, 'bnc_aode'))
  x$.models <- fit_models
  class(x) <- c('bnc_aode_bns', class(x), 'bnc_fit')
  x
}
#' Is it en AODE?
#'
#' @keywords internal
is_aode <- function(x) {
  if (!inherits(x, c('bnc_aode'))) return (FALSE)
  if (length(x$.models) < 2) return (FALSE)
  all(sapply(x$.models, is_ode)) # TODO Should be is spode
}
nmodels <- function(x) {
 stopifnot(inherits(x, 'bnc_aode')) 
 length(x$.models)
}
models <- function(x) { 
 stopifnot(inherits(x, 'bnc_aode'))  
 x$.models
}