# Produces a list of CPTs for the families in the DAG
bnc_bn <- function(x, dataset, smooth, call) {   
  # Check bnc dag
  check_bnc_dag(x) 
  # Get a CPT table for each family
  params <- families2cpts(families(x), dataset, smooth)
  # Currently not forming a grain upon creating the object
  bnc_bn <- make_bnc_bn(x, params = params, grain = NULL, call = call)
  # check
  check_bnc_bn(bnc_bn) 
  # Return
  bnc_bn
}
multi_bnc_bn <- function(x, dataset, smooth, call) {
  x <- ensure_list(x)
  # Check bnc dag
  lapply(x, check_bnc_dag)
  # Check the class is common to all data sets
  class <- get_common_class(x)
  check_class_in_dataset(class, dataset)
  ucpts <- extract_unique_cpts(x, dataset, smooth)
  params_list <- lapply(x, extract_params_cptpool, ucpts)
  # Make a bnc_bn for each x
  bnc_bns <- mapply(make_bnc_bn, x,  params_list, 
                    MoreArgs = list(grain = NULL, call = call),  
                    SIMPLIFY = FALSE)
  lapply(bnc_bns, check_bnc_bn)
  bnc_bns
}
extract_params_cptpool <- function(x, cpt_pool) {
  # Match families to CPTS 
  fams_ids <- make_families_ids(families(x))
  # The following line could be extracted to calling function for speed-up
  cpts_ids <- make_families_ids(lapply(cpt_pool, cpt2family))
  cpt_pool[match(fams_ids, cpts_ids)]
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
  params <- params(x)
  stopifnot(identical(cpts2families(params), families(x)))
  check_anb_cpts(params, class_var(x))
  # Check values
  stopifnot(identical(names(values(x)), names(vars(x))))
}
# Accessors 
params <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  x$.params
}
values <- function(x) {
  cpt_vars_values(params(x))
}
classes <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  cpt_vars_values(params(x))[[class_var(x)]]
}
# Accessors 
#' To grain
#' @export
to_grain <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (is.null(x$.grain))  {
    compile_grain(params(x))  
  }
  else {
    x$.grain  
  }  
}