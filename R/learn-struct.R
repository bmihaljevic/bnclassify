#' @export
nb <- function(class, dataset = NULL, features = NULL) {
  #   # if dataset is provided features is ignored
  if (!is.null(dataset)) {
    features <- get_features(class = class, dataset = dataset)
  }
  dag <- nb_dag(class, features)
  call <- save_bnc_call(match.call(), parent.frame())
  bnc_dag(dag, class = class, call = call)
}
#' @export
fssj <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  just_class_nb <- nb(class = class)
  features <- get_features(class, dataset)
  greedy_search(class = class, to_include = features, init = just_class_nb,
                step = fssj_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
}
#' @export
bsej <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  greedy_search(class = class, to_include = NULL, init = full_nb,
                step = bsej_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
}
#' @export
tanhc <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth)
}
#' @export
tanhc_sp <- function(class, dataset, k, epsilon = 0.01, smooth = 0.01) {    
  full_nb <- nb(class = class, dataset)
  greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode_sp, dataset = dataset, epsilon = epsilon, 
                k = k, smooth = smooth)
}