# a basic supertype of all bnc
bnc_base <- function(class, features) {  
  obj <- list(.class = unname(class))
  obj$.features <- unname(features)
  class(obj) <- 'bnc_base'
  obj
} 
# Creates an augmented naive Bayes with structure but no parameters.
bnc_dag <- function(dag, class) {
  families <- graphNEL2families(dag, class)  
#   Save dag, class, features,and call 
  make_bnc_dag(class = class, families = families, dag = dag)
}
make_bnc_dag <- function(class, families, dag) {
  # Not checking families for efficiency; they are checked in bnc_dag anyway
  obj <- bnc_base(class = class, features = NULL)
  obj$.dag = dag
  obj$.families = families
  class(obj) <- c('bnc_dag', class(obj))
  obj
}
# Checks it is a valid bnc_dag object 
check_bnc_dag <- function(x) {
  check_bnc_dag_basic(x)
  # Check families
  check_anb_families(families(x), class_var(x))  
} 
check_bnc_dag_basic <- function(x) {
  class <- class_var(x)
  features <- features(x)
  # This also checks for class.
  check_features(features = features, class = class)
  stopifnot(identical(vars(x), setNames(nm = c(features, class))))
}

#' @export 
#' @describeIn grain_and_graph Convert to a graphNEL.
as_graphNEL <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  graph_internal2graph_NEL(dag(x))
} 
#' Get underlying graph. This should be exported.
#' @keywords  internal 
#' @param x the bnc object
dag <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.dag
}
#' @export 
#' @describeIn  inspect_bnc_dag Returns the class variable.
class_var <- function(x) {
  stopifnot(inherits(x, "bnc_base"))
  x$.class
}
#' @export 
#' @describeIn  inspect_bnc_dag Returns the features.
features <- function(x) {
  # Implementing a generic features did not allow me to document it in inspect_bnc_dag, so I dispatch by class within the function
  if (inherits(x, 'bnc_dag')) {
    return (setdiff(vars(x), class_var(x)))
  }
  else if (inherits(x, 'bnc_base')) {
    return(x$.features)
  }
  stop('Must be either bnc_dag or bnc_base')
}
#' @export 
#' @describeIn  inspect_bnc_dag Returns all variables (i.e., features + class).
vars <- function(x) {
  setNames(nm = get_family_vars(families(x)))
}
#' @export 
#' @describeIn  inspect_bnc_dag Returns the family of each variable.
families <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.families
} 
#' @export 
#' @describeIn  inspect_bnc_dag Returns the model string of the network in bnlearn format (adding a space in between two families).
modelstring <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  fams <- families(x)
  order <- order_acyclic(families(x))
  fams <- fams[order] 
  paste(sapply(names(fams), function(node) { 
    paste("[", node, ifelse(length(fams[[node]]) - 1 > 0, "|", ""), paste(fams[[node]][-1], sep = "", collapse = ":"), "]", sep = "")
  }), collapse = " ")   
}
# # Returns all feature families excluding the class variable
# # Returns all feature families excluding the class variable
# feature_families <- function(x) {
#   feature_fams <- families(x)[features(x)]
#   lapply(feature_fams, family_features, class_var(x))
# }
#' @export 
#' @describeIn  inspect_bnc_dag Returns the family of each feature.
feature_families <- function(x) {
  families(x)[features(x)]
}