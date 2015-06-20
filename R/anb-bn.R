# Produces a list of CPTs for the families in the DAG
bnc_bn <- function(x, dataset, smooth, call) {   
  # Check bnc dag
  check_bnc_dag(x) 
  # Get a CPT table for each family
  params <- families2cpts(families(x), dataset, smooth)
  # Currently not forming a grain upon creating the object
  bnc_bn <- make_bnc_bn(x, params = params, call = call)
  # check
  check_bnc_bn(bnc_bn) 
  # Return
  bnc_bn
}
multi_bnc_bn <- function(x, dataset, smooth, call) {
  # Unnamed so that it would pass no names to objects created by itearting on it
  x <- ensure_multi_list(x)
  # Check bnc dag
  lapply(x, check_bnc_dag)
  # Check the class is common to all data sets
  class <- get_common_class(x)
  check_class_in_dataset(class, dataset)
  ucpts <- extract_unique_cpts(x, dataset, smooth)
  params_list <- lapply(x, extract_params_cptpool, ucpts)
  # Make a bnc_bn for each x
  bnc_bns <- mapply(make_bnc_bn, x,  params_list, 
                    MoreArgs = list(call = call),  
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
make_bnc_bn <- function(bnc_dag, params, call) {
  bnc_dag$.params <- params
  bnc_dag$.call_bn = call
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
                 graphNEL = to_graphNEL(x))  
  }
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