# Returns families with the class as last node in each.
graph2families <- function(dag, class) {
  #   Check dag and class
  #   Check class is length 1 character  
  check_class(class)
  #   Check dag is graphNEL (or graph adjm?)
  stopifnot(is_dag_graph(dag))
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
  families <- lapply(families, format_family, class)  
  families
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
# Check that the families correspond to an augmented naive Bayes. 
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
make_families_ids <- function(fams) {
  vapply(fams, make_family_id, FUN.VALUE = character(1))
}
make_family_id <- function(fam) {
  paste0(fam, collapse = "")
}
family_features <- function(fam, class) {
  stopifnot(!are_disjoint(fam, class))
  setdiff(fam, class)
}
# Returns the unique families accross these dags. TODO: Standardize families
# first? Does not take into account that same may only differ in the order of
# parents Use duplicated instead of unique in order to preserve the names
unique_families <- function(fams) {
  all_fams <- unlist(fams, recursive = FALSE)
  all_fams[!duplicated(all_fams)]
}
is_dag_families <- function(families) {
  !(is.null(order_acyclic(families)))
}
#' Provide an acyclic ordering (i.e., a topological sort). 
#' @references Beng-Jensen and Gutin, 2007, page 14. 
#' @keywords internal
order_acyclic <- function(families) {
  # Empty list of nodes in acyclic ordering 
  order <- character()  
  not_in_acyc_order <- rep(TRUE, length(families))
  nodes <- get_families_nodes(families)
  # Get the parents here for efficiency
  family_parents <- lapply(families, get_family_parents)
  while (sum(not_in_acyc_order) > 0) {
    # Get nodes not in list and without parents among them
    can_be_added <- vapply(family_parents[not_in_acyc_order], are_disjoint, 
                           nodes[not_in_acyc_order], FUN.VALUE = logical(1))
    if (sum(can_be_added) == 0) { break }
    # Add them to end of the acyclic ordering 
    to_add <- nodes[not_in_acyc_order][can_be_added]
    order <- append(order, to_add)
    not_in_acyc_order <- !(nodes %in% order)
  }
  if (length(nodes) != length(order)) {
  # if (!is_perm(nodes, order)) { Too slow.
    # There is a cycle
    NULL
  }
  else {
    # Return acyclic ordering 
    order
  }
}
get_families_nodes <- function(families) {
  unname(vapply(families, get_family_node, 
                FUN.VALUE = character(1)))
}
get_family_node <- function(family) {
  family[1]
}
get_family_parents <- function(family) {
  family[-1]
}
#' Based on gRbase::ancestors()
#' @keywords internal
get_ancestors <- function(node, families) {
  stopifnot(node %in% names(families))
  find_my_parents <- node 
  ancestors <- character()
  # while find_my_parents not empty
  while (length(find_my_parents) > 0) {
    # get unique parents of all find_my_parents 
    parents <- unique(unlist(lapply(families[find_my_parents], 
                                    get_family_parents), use.names = FALSE))
    # set find_my_parents ot parents that are not in ancestors
    new_parents <- setdiff(parents, ancestors)
    find_my_parents <- new_parents 
    # add the new parents to ancestors
    ancestors <- append(ancestors, new_parents)
  }
  ancestors
}