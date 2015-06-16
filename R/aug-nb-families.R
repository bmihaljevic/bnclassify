# Already here check that the families are ANB?
# Or in check_families and remove the check from here.

get_family_vars <- function(families) {
  names(families)
}
# Check that the last family is class. I.e., families are ordered. 
check_anb_families <- function(families, class) {
  vars <- get_family_vars(families)
  stopifnot(is_non_empty_complete(vars))
  # Class must be the last family so that it is also last in vars()
  stopifnot(is_last(class, vars))
  # check each family
  fams_ok <- all( mapply(is_anb_family, families, vars, 
                    MoreArgs = list(class = class), SIMPLIFY = TRUE) )
  stopifnot(fams_ok)
}
# All elements are non empty characters
# The var is the 1 element of the family, class is the last
is_anb_family <- function(family, var, class) {
  is.character(family) && is_non_empty_complete(family) && is_last(class, family) && is_at_pos(var, 1, family)
}


# make_anb_family

# # Returns all families excluding the class variables
# bnc_feature_families <- function(x) {
#   stopifnot(inherits(x, "bnc_dag"))
#   lapply(bnc_families(x), setdiff, bnc_class(x))
# }


# check_families <- function(families, vars, class) {
#   #   Check all families are character 
#   stopifnot(is.list(families))
#   stopifnot(identical(Filter(is.character, families), families))
#   #   Check names of families  vars
#   stopifnot(identical(names(families), names(vars)))
#   # Check last element in each family is class  
#   last <- vapply(families, get_last, FUN.VALUE = character(1))
#   stopifnot(all(last == class, na.rm = FALSE)) # NA will be resolved as false
#   # Check first element in each family is the variable 
#   first <- vapply(families, get_null_safe, 1, FUN.VALUE = character(1))
#   stopifnot(identical(first, vars))
# }
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
  # lapply(families, make_last, class)  
  families
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
# # Ensure x is a list
# if (!is_just(x, "list")) {
#   x <- list(x)
# }
# fams <- lapply(x, bnc_families)