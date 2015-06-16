# Returns families with the class as last node in each.
graph2families <- function(dag, class) {
  #   Check dag and class
  #   Check class is length 1 character  
  check_class(class)
  #   Check dag is graphNEL (or graph adjm?)
  check_dag(dag) # TODO: May comment out this check because it slows down.
  #   Check class is in nodes of dag  
  stopifnot(class %in% graph::nodes(dag)) # TODO: call basic-dag here
  #   Features = all nodes other than class
  # TODO: call basic-dag here
  features <- setdiff(graph::nodes(dag), class)
  #  Save features and class in vars vector 
  vars <- setNames(nm = c(features, class))
  #  For each var, extract the family in the dag
  families <- lapply(vars, family, dag)  
  # Make class last element of each family 
  lapply(families, format_family, class)  
}
# Ensures class is last.
# TODO: maybe should ensure an alphabetic ordering of non-class parents
format_family <- function(family, class) {
  make_last(family, class)
}
# Returns the variables whose families are these
get_family_vars <- function(families) {
  names(families)
}
# Check that the last family is class. I.e., families are ordered. 
check_anb_families <- function(families, class) {
  vars <- get_family_vars(families)
  stopifnot(is_non_empty_complete(vars))
  # Class must be the last family so that it is also last in vars()
  stopifnot(is_last(class, vars))
  #   Check that class has no parents
  stopifnot(identical(families[[class]], class))
  # check each family
  fams_ok <- all( mapply(is_anb_family, families, vars, 
                    MoreArgs = list(class = class), SIMPLIFY = TRUE) )
  #   Not checking for cycles though...
  stopifnot(fams_ok)
}
# All elements are non empty characters
# The var is the 1 element of the family, class is the last
is_anb_family <- function(family, var, class) {
  is.character(family) && is_non_empty_complete(family) && is_last(class, family) && is_at_pos(var, 1, family)
}
# Returns bnc_families but with a unique name
tag_families <- function(fams) {
  ids <- lapply(fams, paste0, collapse = "")
  setNames(fams, nm = ids)
}
# Returns the unique families accross these dags. 
unique_families <- function(fams) {
  all_fams <- unlist(fams, recursive = FALSE)
  # Does not take into account that same may only differ in the order of parents
  # Use duplicated instead of unique in order to preserve the names
  all_fams[!duplicated(all_fams)]
}