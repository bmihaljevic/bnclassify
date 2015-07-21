# Produces a list of CPTs for the families in the DAG
bnc_bn <- function(x, dataset, smooth) {   
    # Get a CPT table for each family
  params <- families2cpts(families(x), dataset, smooth)
  bnc_bn <- make_bnc_bn(x, params = params)
  check_bnc_bn(bnc_bn) 
  bnc_bn
}
make_bnc_bn <- function(bnc_dag, params) {
  bnc_dag$.params <- params
  class(bnc_dag) <- c('bnc_bn', class(bnc_dag))
  bnc_dag
}
bn2dag <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  if (is_just(x, "bnc_dag")) {
    x
  }
  else {
    make_bnc_dag(class = class_var(x), families = families(x), 
                 graphNEL = as_graphNEL(x))  
  }
}
check_bnc_bn <- function(x) {
  check_bnc_dag(x)
  # Check CPT families match original families equal to the families of bnc 
  params <- params(x)
  stopifnot(identical(cpts2families(params), families(x)))
  stopifnot(identical(names(values(x)), names(vars(x))))
}
# Accessors 

#' @export 
#' @describeIn bnc_bn_object Returns the list of CPTs, in the same order as \code{\link{vars}}.
params <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  x$.params
}
#' @export 
#' @describeIn bnc_bn_object Returns the possible values of each variable, in the same order as \code{\link{vars}}.
values <- function(x) {
  cpt_vars_values(params(x))
}
#' @export 
#' @describeIn bnc_bn_object Returns the possible values of the class variable.
classes <- function(x) {
  values(x)[[class_var(x)]]
}