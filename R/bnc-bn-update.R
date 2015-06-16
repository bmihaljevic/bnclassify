# bnc_wrap <- function(x, awnb, blacklist) {
#   ...TODO
# x
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
multi_learn_predict <- function(dags, dataset, smooth, prob = FALSE) {
  # Ensure it is a list
  if (!is_just(dags, "list")) {
    dags <- list(dags)
  }
  xfams_list <- lapply(dags, feature_families)
  # Get the unique families. TODO: Standardize families first?
  uxfams <- unique_families(xfams_list)
  # Name the entires in ufams with families ids
  names(uxfams) <- vapply(uxfams, make_family_id, FUN.VALUE = character(1))
  # Replace families with theid ids 
  xfams_ids <- lapply(xfams_list, make_families_ids)
#   Extract the CPT of each unique feature family
  uxcpts <- families2cpts(uxfams, dataset = dataset, smooth = smooth)
  # Extract class CPT 
  class <- class_var(dags[[1]])
  cp <- extract_cpt(class, dataset = dataset, smooth = smooth)
  compute_augnb_lucp_multi(class, xfams_ids, uxcpts, cp, dataset = dataset)
  #   map
}