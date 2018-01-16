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
# Optionally updates the dag prior to updating the parameters.
update <- function(x, dataset, dag) {
  stopifnot(is.logical(dag))
  dg <- NULL
  if (dag) {
    dg <- update_dag(x, dataset)
  }
  else {
    dg <- bn2dag(x)
  }
  lp_args <- get_lp_update_args(x)
  update_lp(dag = dg, lp_fargs = lp_args, dataset = dataset)
}
save_bnc_call <- function(fun_name, call, env) {
  stopifnot(is.character(fun_name))
  call[[1]] <- fun_name
  # To make sure that this dataset is not accidentaly used on updates.
  call['dataset'] <- NULL
  lapply(call, eval, envir = env)
}
do_bnc_call <- function(fargs, dataset) {
  fargs$dataset <- dataset
  call <- make_call(fargs[[1]], fargs[-1])
  eval(call)
}
add_dag_call_arg <- function(bnc_dag, fun_name, call, env, force = FALSE) {
  add_call_arg(bnc_dag, fun_name, call, env, arg = '.call_struct', force = force)
}
remove_dag_call_arg <- function(bnc_dag) {
  bnc_dag[['.call_struct']] <- NULL
  bnc_dag
}
add_params_call_arg <- function(bnc_bn, call, env, force = TRUE) {
  add_call_arg(bnc_bn, 'lp', call, env, arg = '.call_bn', force = force)
}
add_call_arg <- function(bnc_dag, fun_name, call, env, arg, force) {
  # stopifnot(inherits(bnc_dag, "bnc_dag"))
  # TODO  Fix this for appropriate types
  stopifnot(inherits(bnc_dag, "bnc_dag") || inherits(bnc_dag, "bnc_base"))
  if (!force) { 
    stopifnot(is.null(bnc_dag[[arg]]))
  }
  bnc_dag[[arg]] <- save_bnc_call(fun_name, call, env)
  bnc_dag
}
get_lp_update_args <- function(x) {
  stopifnot(!is.null(x$.call_bn))
  x$.call_bn
}
get_dag_update_args <- function(x) {
  stopifnot(!is.null(x$.call_struct))
  x$.call_struct
}
update_dag <- function(x, dataset)  {
  do_bnc_call(get_dag_update_args(x), dataset)
}
update_lp <- function(dag, lp_fargs, dataset) {
  lp_fargs$x <- dag
  do_bnc_call(lp_fargs, dataset)
}