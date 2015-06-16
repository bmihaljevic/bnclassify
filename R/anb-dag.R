#' Creates an augmented naive Bayes with structure but no parameters.
bnc_dag <- function(dag, class, call) {
  families <- graph2families(dag, class)  
#   Save dag, class, features,and call 
  obj <- make_bnc_dag(class = class, families = families, graphNEL = dag, 
                      call = call)
  check_bnc_dag(obj)
  obj
}
make_bnc_dag <- function(class, families, graphNEL, call) {
  # Not checking families for efficiency; they are checked in bnc_dag anyway
  obj <- list(.dag = graphNEL, .class = class, .families = families, 
              .call_struct = call)
  class(obj) <- 'bnc_dag'
  obj
}
#' Checks it is a valid bnc_dag object 
check_bnc_dag <- function(x) {
  class <- bnc_class(x)
  features <- bnc_features(x)
  check_class(class)
  check_features(features = features, class = class)
  stopifnot(identical(bnc_vars(x), setNames(nm = c(features, class))))
  # Check families
  families <- bnc_families(x)
  check_anb_families(families, class)  
  # Check dag is a graphNEL.
  # stopifnot(inherits(to_graphNEL(x), "graphNEL"))
} 
# Accessor functions 
to_graphNEL <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.dag
}
bnc_class <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.class
}
bnc_features <- function(x) {
  setdiff(bnc_vars(x), bnc_class(x))
}
bnc_vars <- function(x) {
  setNames(nm = get_family_vars(bnc_families(x)))
}
bnc_families <- function(x) {
  stopifnot(inherits(x, "bnc_dag"))
  x$.families
}