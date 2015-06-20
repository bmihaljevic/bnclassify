#' @export
nb <- function(class, dataset = NULL, features = NULL) {
  #   # if dataset is provided features is ignored
  if (!is.null(dataset)) {
    features <- get_features(class = class, dataset = dataset)
  }
  nb <- bnc_dag(nb_dag(class, features), class)
  add_dag_call_arg(nb, call = match.call(), env = parent.frame())
}
#' Learns Bayesian network classifiers in a wrapper fashion.
#' 
#' @references Pazzani M (1996). Constructive induction of Cartesian product 
#'   attributes. In \emph{Proceedings of the Information, Statistics and 
#'   Induction in Science Conference (ISIS-1996)}, pp. 66-77
#' @param class A character. Name of the class variable.
#' @param epsilon A numeric. Minimum absolute improvement required to keep
#'   searching.
#' @export
fssj <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  just_class_nb <- nb(class = class)
  features <- get_features(class, dataset)
  x <- greedy_search(class = class, to_include = features, init = just_class_nb,
                step = fssj_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
  add_dag_call_arg(x, call = match.call(), env = parent.frame(), force = TRUE)
}
#' @rdname fssj
#' @export
bsej <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = bsej_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
  add_dag_call_arg(x, call = match.call(), env = parent.frame())
}
#' @export
tanhc <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
  add_dag_call_arg(x, call = match.call(), env = parent.frame(), force = TRUE)
}
#' @export
tanhc_sp <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode_sp, dataset = dataset, epsilon = epsilon, 
                k = k, smooth = smooth)
  add_dag_call_arg(x, call = match.call(), env = parent.frame(), force = TRUE)
}
#' @export
tan_bnc <- function(class, dataset, score='loglik', blacklist = NULL, 
                    root = NULL) {
  x <- chowliu(class, dataset, score = score, blacklist = blacklist, 
          root = root)
  add_dag_call_arg(x, call = match.call(), env = parent.frame(), force = TRUE)
}