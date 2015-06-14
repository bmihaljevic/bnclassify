#' Creates a Bayesian network classifier with structure but no parameters.
#' 
#' Must be an augmented naive Bayes.
bnc_dag <- function(dag, class, call) {
#   Check dag and class
  check_dag_class(dag, class)
#   Features = all nodes other than class
  features <- setdiff(graph::nodes(dag), class)
#  Save features and class in vars vector 
  vars <- setNames(nm = c(features, class))
#  For each var, extract the family in the dag
  families <- lapply(vars, family, dag)  
#  Check class is in every family (augmented naive Bayes)
  class_in_families <- Reduce(intersect, families, init = class)
  stopifnot(identical(class_in_families, class))
# Make class last element of each family 
  families <- lapply(families, make_last, class)  
#   Save dag, class, features,and call 
  obj <- list(.dag = dag, .class = class, .features = features, .vars = vars,
              .families = families, .call_struct = call)
#   Set class: bnc_dag
  class(obj) <- 'bnc_dag'
  check_bnc_dag(obj)
#   return object
  obj
}
#' Checks the dag and class are adequate for a Bayesian network classifier. 
check_dag_class <- function(dag, class) {
#   Check class is length 1 character  
  check_node(class)
#   Check dag is graphNEL (or graph adjm?)
  check_dag(dag)
#   Check class is in nodes of dag  
  stopifnot(class %in% graph::nodes(dag))
}
#' Checks it is a valid bnc_dag object 
check_bnc_dag <- function(x) {
  # Check it is a bnc_dag
  stopifnot(inherits(x, "bnc_dag"))
  # Check class
  class <- bnc_class(x)
  check_class(class)
  # Check features
  features <- bnc_features(x)
  check_features(features, class)
  # Check vars
  vars <- bnc_vars(x)
  check_vars(vars, features, class)
  # Check families
  families <- bnc_families(x)
  check_families(families, vars, class)  
  # Check dag is a graphNEL.
  stopifnot(inherits(bnc_get_dag(x), "graphNEL"))
} 
#' Check bnc_vars is features + class. 
check_vars <- function(vars, features, class) {
  check_class(class)
  check_features(features = features, class = class)
  stopifnot(identical(vars, setNames(nm = c(features, class))))
}
# Check families
check_families <- function(families, vars, class) {
  #   Check all families are character 
  stopifnot(is.list(families))
  stopifnot(identical(Filter(is.character, families), families))
  #   Check names of families  vars
  stopifnot(identical(names(families), names(vars)))
  # Check last element in each family is class  
  last <- vapply(families, get_last, FUN.VALUE = character(1))
  stopifnot(all(last == class, na.rm = FALSE)) # NA will be resolved as false
  # Check first element in each family is the variable 
  first <- vapply(families, get_null_safe, 1, FUN.VALUE = character(1))
  stopifnot(identical(first, vars))
}
# Accessor functions 
bnc_get_dag <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.dag
}
bnc_class <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.class
}
bnc_features <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.features
}
bnc_vars <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.vars
}
bnc_families <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.families
}
# Returns all families excluding the class variables
bnc_feature_families <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  lapply(bnc_families(x), setdiff, bnc_class(x))
}