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

# =========================
# Useful

# bnc_wrap <- function(x, awnb, blacklist) {
#   ...TODO
# x
# }

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
add_dag_call_arg <- function(bnc_dag, call, env, force = FALSE) {
  add_call_arg(bnc_dag, call, env, arg = '.call_struct', force = force)
}
add_params_call_arg <- function(bnc_bn, call, env, force = TRUE) {
  add_call_arg(bnc_bn, call, env, arg = '.call_bn', force = force)
}
add_call_arg <- function(bnc_dag, call, env, arg, force) {
  stopifnot(inherits(bnc_dag, "bnc_dag"))
  if (!force) { 
    stopifnot(is.null(bnc_dag[[arg]]))
  }
  bnc_dag[[arg]] <- save_bnc_call(call, env)
  bnc_dag
}
# Strip the function name and the graph.  Function must be lp.
get_lp_multi_update_args <- function(x) {
  args <- x$.call_bn
  stopifnot(!is.null(args))
  stopifnot(as.character(args[[1]]) %in% c('lp', 'lpawnb'))
  args <- args[-1]
  args$x <- NULL
  args
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

multi_update <- function(x, dataset, dag, smooth = NULL) {
# The lp args must be either smooth or within the x objects: 
  if (is.null(smooth)) {
    x <- ensure_list(x, type = "bnc_bn")  
  }
  else {
    x <- ensure_list(x)  
  }
  dags <- NULL
  if (dag) {
    dags <- lapply(x, update_dag, dataset)
  }
  if (is.null(dags)) {
    dags <- lapply(x, bn2dag)
  }
  # smooth overrides lp args that may be in x 
  if (!is.null(smooth)) {
    multi_bnc_bn(dags, dataset, smooth = smooth)
  }
  else {
    lp_multi_args <- lapply(x, get_lp_multi_update_args)
    if (are_all_equal(lp_multi_args)) {
      # If all lp args are the same, then can call multi_bnc_bn
      # TODO: use weights and awnb if needed.
      multi_bnc_bn(dags, dataset, smooth = lp_multi_args[[1]]$smooth)
    }
    else {
      lp_args <- lapply(x, get_lp_update_args)
      mapply(update_lp, dags, lp_args, MoreArgs = list(dataset = dataset), 
             SIMPLIFY = FALSE)
    }  
  }
}