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
# do_bnc_call <- function(lcall, args) {
#   call <- pryr::modify_call(lcall$call, args)
#   eval(call, lcall$env)
# }
# save_bnc_call <- function(call, env) {
#   list(call=call, env=env)
# }
# check_bnc_call <- function(lcall) {
#   stopifnot(is.call(lcall$call))
#   stopifnot(is.environment(lcall$env))
# }