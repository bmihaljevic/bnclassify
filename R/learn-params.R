#' Learn parameters.
#' @export
lp <- function(x, dataset, smooth) {
  call <- save_bnc_call(match.call(), parent.frame())
  bnc_bn(x, dataset, smooth, call = call)
}