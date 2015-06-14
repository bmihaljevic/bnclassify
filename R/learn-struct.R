# Tener todas las front-end aquí? Las documento aquí tb? Así a golpe de vista sé qué hay y cual es el formato. Qué cambiaría en ODE? qué es común a todas? Que extraen las features, por ejemplo?


# nb <- function(class, dataset = NULL, features = NULL) {
#   # if dataset is provided features is ignored
#   if (!is.null(dataset)) {
#     features <- get_features(class = class, dataset = dataset)
#   }


#' @export
nb <- function(class, dataset) {
  features <- get_features(class = class, dataset = dataset)
  dag <- nb_dag(class, features)
  call <- save_bnc_call(match.call(), parent.frame())
  bnc_dag(dag, class = class, call = call)
}
# debug = FALSE
fssj <- function(class, dataset, k, epsilon = 0.01) {    
  just_class_nb <- nb_dag(class = class, features = NULL)
  # search state could use the graphNEL directly?
  just_class <- search_state(just_class_nb, potential_nodes = features)  
  a <- greedy_search(start = just_class, move = inclusions_and_joining_inclusions, 
                     dataset = dataset, score = score, epsilon = epsilon, 
                     debug = debug)
  a$structure
}