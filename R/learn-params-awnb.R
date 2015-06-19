#' Compute feature weights according to the AWNB method.
#' 
#' @export
#' @references Mark Hall (2004). A decision tree-based attribute weighting 
#'   filter for naive Bayes. \emph{Knowledge-based Systems}, \bold{20}(2), 
#'   120-126.
awnb <- function(class, dataset, bootstrap_size = 0.5, trees = 10) {
  subsamples <- replicate(trees, bootstrap_ss(dataset, bootstrap_size), 
                          simplify = FALSE)
  Wtrees <- lapply(subsamples, learn_unprunned_tree, class)
  depths <- lapply(Wtrees, identify_min_testing_depths)
  depths <- unlist(depths, use.names = TRUE)
  if (length(depths) == 0) stop ("Only empty trees have been learned.")
  tapply(depths, names(depths), function(x) sum(x ^ -0.5), simplify = TRUE) / trees
}
# For each tree, get a sample with bootstrap 
# From each sample, learn tree
# For each tree get weights 
# Average the weights across the features
# Return a named vector 

#' Identifies the lowest (closest to root) depths at which the features of a 
#' classification tree are tested.
#' 
#' @keywords internal
identify_min_testing_depths <- function(tree) {
  depths <- identify_all_testing_depths(tree)
  if (length(depths) == 0) return(NULL)
  stopifnot(length(names(depths)) > 0)
  sort(tapply(depths, names(depths), min))
}