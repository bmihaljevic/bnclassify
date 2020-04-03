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
#' Returns a \code{c("bnc_multinet", "bnc")} object.
#' @keywords internal
bnc_multinet_tan <- function(class, dataset, features) {
  if (!is.null(dataset)) {
    features <- get_features(class = class, dataset = dataset)
  }
  if (length(features) == 1){ 
    return(nb(class = class, features = features))
  }
  names(features) <- features
  datasets <- split(dataset, dataset[[class]])
  models <- vector("list")
  for (i in levels(dataset[[class]])){
    models[[i]]<-tan_cl("class", datasets[[i]])}
  stopifnot(length(models) > 0)
  stopifnot(all(vapply(models, is_ode, FUN.VALUE = logical(1))))
  bnc <- bnc_base(class = class, features = features)
  bnc$.models <- models
  class(bnc) <- c('bnc_multinet', class(bnc))
  bnc
}
#' Fits an multinet model.
#' @keywords internal
bnc_multinet_bns <- function(x, fit_models) {
  stopifnot(inherits(x, 'bnc_multinet'))
  x$.models <- fit_models
  class(x) <- c('bnc_multinet_bns', class(x), 'bnc_fit')
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
#' Is it an ensemble?
#'
#' @keywords internal
is_ensemble <- function(x) {
  is_aode(x) || is_multinet(x)  
}
is_multinet <- function(x) {
 inherits(x, "bnc_multinet") 
}
nmodels <- function(x) {
 stopifnot(inherits(x, 'bnc_aode') || inherits(x, "bnc_multinet")) 
 length(x$.models)
}
models <- function(x) { 
 stopifnot(inherits(x, 'bnc_aode') || inherits(x, "bnc_multinet"))  
 x$.models
}