#' Learn parameters.
#' @export
lp <- function(x, dataset, smooth, weights = NULL) {
  call <- save_bnc_call(match.call(), parent.frame())
  bn <- bnc_bn(x, dataset, smooth, call = call)
  if (!is.null(weights)) {
    bn <- set_weights(bn, weights)
  }
  bn
}
set_weights <- function(bn, weights) {
  # Currently only expecting weights that can decrease the importance of a feature
  stopifnot(is_non_empty_complete(weights))
  stopifnot(all(weights >= 0 & weights <= 1))
  # Check weights correspond to features
  feats <- names(weights)
  stopifnot(is_non_empty_complete(feats), all(feats %in% features(bn)))
  # modify cpts directly
  bn$.params[feats] <- 
    mapply(exponentiate_cpt, bn$.params[feats], weights)
  bn
}