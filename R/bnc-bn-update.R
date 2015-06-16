# bnc_wrap <- function(x, awnb, blacklist) {
#   ...TODO
# x
# }

# # Repeats the procedure used to learn x on \code{dataset}  
# bnc_update <- function(x, dataset, dag = FALSE) {
#   args <- bnc_get_update_args(x, dag = dag)
#   args$dataset <- dataset
#   stopifnot(is_subset(c('lp_fargs', 'dataset'), names(args)))
#   do.call('bnc_update_args', args)
# }
# Arguments to be passed to bnc_update_args()
bnc_get_update_args <- function(x, dag) {
  stopifnot(is.logical(dag))
  args <- list(lp_fargs = x$.call_bn)
  # lp_fargs must always be present
  stopifnot(!is.null(args$lp_fargs))
  # If dag then include dag arguments
  if (dag) {
    args$dag_fargs <- x$.call_struct
    stopifnot(!is.null(args$dag_fargs))
  }
  args
}
# Updates the dag in the lp_args.
# No function name is passed here.
make_daglp_updateable <- function(x, lp_args) {
  stopifnot(is.list(lp_args))
  lcall <- list('lp', x = x)
  lcall <- append(lcall, lp_args)
  list(.call_bn = lcall)
}
bnc_update <- function(args, dataset) {
  bnc_update_args(args$lp_fargs, dataset, args$dag_fargs)
}
bnc_update_args <- function(lp_fargs, dataset, dag_fargs = NULL) {
  # If dag needs to be called, call it first then feed it into lp arguments
  if (!is.null(dag_fargs)) {
    # dag_fargs contain both function name and arguments. 
    dag <- do_bnc_call(dag_fargs, dataset)
    lp_fargs$x <- dag
  }
  # Wrap the result of lp before it's returned 
  res <- do_bnc_call(lp_fargs, dataset)
  # bnc_wrap(res) TODO
  res
}
save_bnc_call <- function(call, env) {
  call[[1]] <- as.character(call[[1]])
  # To make sure that this dataset is not accidentaly used on updates.
  call['dataset'] <- NULL
  lapply(call, eval, envir = env)
}
do_bnc_call <- function(fargs, dataset) {
  fargs$dataset <- dataset
  call <- pryr::make_call(fargs[[1]], .args = fargs[-1])
  eval(call)
}
# multi_learn_predict <- function(x, cp, classes, lp_args, prob = FALSE) {
multi_learn_predict <- function(x, smooth, dataset, prob = FALSE) {
#   Get the probabilities entries for each features CPT
  list_of_cpts <- multi_learn(x, smooth, dataset)
#   Get unnormalized log class posterior for each list of cpts (each dag)
  compute_augnb_lucp_multi(list_of_cpts, class_var(x), dataset)
#   For each x, 
}
multi_learn <- function(x, smooth, dataset) {
  # x is a list of bnc_dag. Ensure it is a list.
  # Get all families, including that of the class, for each x
  families_list <- lapply(x, families)
  # Assign unique id to each family
  families_list <- lapply(families_list, tag_families)
  # Get the unique families. TODO: Standardize families first?
  ufams <- unique_families(families_list)
#   Extract the CPT of each unique family
  ucpts <- lapply(ufams, extract_cpt, dataset, smooth = smooth)
#   Return the list of the cpts. 
  lapply(families_list, function(dag_fams) ucpts[names(dag_fams)])
#     Could return a bnc_bn object: cpts + class. This would also need to ensure an  
#     order of the variables.
}
unique_cpts <- function() {
  # ... 
}

# multi_learn_predict (dags, dataset) {
#   tag the families in each dag
#   get unique families
#   get cpts for unique families
#   get unique cpts 
#   multi posterior (tagged_dags, unique_cpts, dataset)
#   map
# }