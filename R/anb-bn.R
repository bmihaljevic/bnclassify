# Produces a list of CPTs for the families in the DAG
bnc_bn <- function(x, dataset, smooth, call) {   
  # Check bnc dag
  check_bnc_dag(x) 
  # Get a CPT table for each family
  params <- families2cpts(bnc_class(x), bnc_families(x), dataset, smooth)
  # Currently not forming a grain upon creating the object
  bnc_bn <- make_bnc_bn(x, params = params, grain = NULL, call = call)
  # check
  check_bnc_bn(bnc_bn) 
  # Return
  bnc_bn
}
make_bnc_bn <- function(bnc_dag, params, grain, call) {
  bnc_bn <- append(bnc_dag, list(.params = params, .grain = NULL, 
                                 .call_bn = call))
  class(bnc_bn) <- c('bnc_bn', class(bnc_dag))
  bnc_bn
}
check_bnc_bn <- function(x) {
  # Check it is a bnc dag
  check_bnc_dag(x)
  # Check CPT families match original families equal to the families of bnc 
  params <- bnc_params(x)
  stopifnot(identical(cpts2families(params), bnc_families(x)))
  check_anb_cpts(params, bnc_class(x))
  # Check values
  stopifnot(identical(names(bnc_values(x)), names(bnc_vars(x))))
}
# Accessors 
bnc_params <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  x$.params
}
bnc_values <- function(x) {
  cpt_vars_values(bnc_params(x))
}
bnc_classes <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  cpt_vars_values(bnc_params(x))[[bnc_class(x)]]
}
# Accessors 
#' To grain
#' @export
to_grain <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (is.null(x$.grain))  {
    compile_grain(bnc_params(x))  
  }
  else {
    x$.grain  
  }  
}