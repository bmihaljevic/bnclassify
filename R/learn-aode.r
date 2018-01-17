#' Learn an AODE ensemble.
#' 
#' If there is a single predictor then returns a naive Bayes.
#' 
#' @param m integer The minimum frequency of an value \eqn{x_i} in order to use 
#'   SPODE \eqn{i} in the ensemble.
#' @keywords internal
#' @return \code{bnc_aode} or \code{bnc_str}
aode <- function(class, dataset, m = 1, ...) {      
  # TODO: what id data is null? see also in nb().
  features <- get_features(class = class, dataset = dataset)
  if (length(features) == 1) return(nb(class = class, features = features))
  names(features) <- features
  models <- lapply(features, spode, features, class)
  bnc_aode_str(models = models, m = m, class = class, features = features)
}  
#' Returns a Superparent one-dependence estimator. 
#' 
#' @param sp character The superparent.
#' @keywords internal
spode <- function(sp, features, class) {
  stopifnot(length(sp) == 1)
  stopifnot(length(class) == 1)
  stopifnot(class != sp)
  stopifnot(!class %in% features)
  features <- setdiff(features, sp)
  features_graph <- nb_dag(class = sp, features = features)
  dag <- superimpose_node(node = class, dag = features_graph) 
  bnc_dag(dag = dag, class = class)
} 