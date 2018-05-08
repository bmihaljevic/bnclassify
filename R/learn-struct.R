#' Learn a naive Bayes network structure.
#' 
#' Learn a naive Bayes network structure.
#' 
#' @export
#' @param class A character. Name of the class variable.
#' @param dataset The data frame from which to learn the classifier.
#' @param features  A character vector. The names of the features. This argument
#'   is ignored if \code{dataset} is provided.
#' @return A \code{\link{bnc_dag}} object.
#' @examples  
#' data(car)
#' nb <- nb('class', car)   
#' nb2 <- nb('class', features = letters[1:10])
#' \dontrun{plot(nb2)}
nb <- function(class, dataset = NULL, features = NULL) {
  #   # if dataset is provided features is ignored
  if (!is.null(dataset)) {
    features <- get_features(class = class, dataset = dataset)
  }
  nb <- bnc_dag(nb_dag(class, features), class)
  add_dag_call_arg(nb, 'nb', call = match.call(), env = parent.frame())
}
#' @export
#' @rdname greedy_wrapper
fssj <- function(class, dataset, k, epsilon = 0.01, smooth = 0, 
                 cache_reset = NULL) {    
  just_class_nb <- nb(class = class)
  # Use just the structure, not the call 
  just_class_nb <- remove_dag_call_arg(just_class_nb)
  features <- get_features(class, dataset)
  x <- greedy_search(class = class, to_include = features, init = just_class_nb,
                step = fssj_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth, cache_reset = cache_reset)
  add_dag_call_arg(x, fun_name = 'fssj', call = match.call(), 
                   env = parent.frame(), force = TRUE)
}
#' @rdname greedy_wrapper
#' @export
bsej <- function(class, dataset, k, epsilon = 0.01, smooth = 0,
                 cache_reset = NULL) {    
  full_nb <- nb(class = class, dataset)
  full_nb <- remove_dag_call_arg(full_nb)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = bsej_step, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth, cache_reset = cache_reset)
  add_dag_call_arg(x, fun_name = 'bsej', call = match.call(), env = parent.frame())
}
#' @export
#' @rdname greedy_wrapper
tan_hc <- function(class, dataset, k, epsilon = 0.01, smooth = 0,
                   cache_reset = NULL) {    
  full_nb <- nb(class = class, dataset)
  full_nb <- remove_dag_call_arg(full_nb)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode, dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth, cache_reset = cache_reset)
  add_dag_call_arg(x, fun_name = 'tan_hc', call = match.call(), 
                   env = parent.frame(), force = TRUE)
} 
#' @export
#' @rdname greedy_wrapper
kdb <- function(class, dataset, k, kdbk = 2, epsilon = 0.01, smooth = 0,
                   cache_reset = NULL) {    
  full_nb <- nb(class = class, dataset)
  full_nb <- remove_dag_call_arg(full_nb)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_kdb(kdbk), dataset = dataset, epsilon = epsilon, k = k,
                smooth = smooth, cache_reset = cache_reset)
  add_dag_call_arg(x, fun_name = 'kdb', call = match.call(), 
                   env = parent.frame(), force = TRUE)
}
#' @export
#' @export
#' @rdname greedy_wrapper
tan_hcsp <- function(class, dataset, k, epsilon = 0.01, smooth = 0,
                     cache_reset = NULL) {    
  full_nb <- nb(class = class, dataset)
  full_nb <- remove_dag_call_arg(full_nb)
  x <- greedy_search(class = class, to_include = NULL, init = full_nb,
                step = augment_ode_sp, dataset = dataset, epsilon = epsilon, 
                k = k, smooth = smooth, cache_reset = cache_reset)
  add_dag_call_arg(x, fun_name = 'tan_hcsp', call = match.call(), env = parent.frame(), 
                   force = TRUE)
}
#' @export
#' @rdname tan_chowliu
tan_cl <- function(class, dataset, score='loglik', root = NULL) {
  x <- chowliu(class, dataset, score = score, blacklist = NULL, 
          root = root)
  add_dag_call_arg(x, fun_name = 'tan_cl', call = match.call(), 
                   env = parent.frame(), force = TRUE)
}
#' Learn an AODE ensemble.
#' 
#' If there is a single predictor then returns a naive Bayes.
#' 
#' @export
#' @inheritParams nb
#' @return A \code{bnc_aode} or a \code{bnc_dag} (if returning a naive Bayes)
aode <- function(class, dataset, features = NULL) {       
  if (!is.null(dataset)) {
    features <- get_features(class = class, dataset = dataset)
  }
  if (length(features) == 1) return(nb(class = class, features = features))
  names(features) <- features
  models <- lapply(features, spode, features, class)
  x <- bnc_aode(models = models, class_var = class, features = features) 
  add_dag_call_arg(x, fun_name = 'aode', call = match.call(), 
                   env = parent.frame(), force = TRUE)
}  