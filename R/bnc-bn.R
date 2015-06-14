# Produces a list of CPTs and possible values for DAG variable  
bnc_bn <- function(x, dataset, smooth, call) {   
  # Check bnc dag
  check_bnc_dag(x) 
  # Check dataset 
  check_dataset(dataset)
  # Get a CPT table for each family
  families <- bnc_families(x)
  params <- lapply(families, extract_cpt, dataset, smooth = smooth)
  # Create object
  # Currently not forming a grain upon creating the object
  bnc_bn <- append(x, list(.params = params, .grain = NULL, .call_bn = call))
  class(bnc_bn) <- c('bnc_bn', class(x))
  # check
  check_bnc_bn(bnc_bn) 
  # Return
  bnc_bn
}
check_bnc_bn <- function(x) {
  # Check it is a bnc_bn
  stopifnot(inherits(x, "bnc_bn"))
  # Check it is a bnc dag
  check_bnc_dag(x)
  # Check names of CPT dims equal to the families of bnc 
  params <- bnc_params(x)
  dnames <- lapply(params, function(cpt) names(dimnames(cpt)))
  families <- bnc_families(x)
  stopifnot(identical(families, dnames)) 
  # Check values
  stopifnot(identical(names(bnc_values(x)), names(bnc_vars(x))))
  #Check values correspond to the 1st dim of the CPTs
  check_cpts(x)
}
# Checks cpts ordered according to bnc_vars and 1D names correspond to bnc_vars()
check_cpts <- function(x) {
  # Check it is a bnc_bn
  stopifnot(inherits(x, "bnc_bn"))
  cpts <- bnc_params(x)
  # TODO: see new-design.md for cpt module
  # Check the names of cpts are equal to the names of the vars
  vars <- names(cpts)
  stopifnot(identical(vars, names(bnc_vars(x))))
  # Checks 1D corresponds to vars
  cpt_vars_values(bnc_params(x))
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