#' Compute feature weights according to the AWNB method.
#' 
#' @export
#' @references Mark Hall (2004). A decision tree-based attribute weighting 
#'   filter for naive Bayes. \emph{Knowledge-based Systems}, \bold{20}(2), 
#'   120-126.
awnb <- function(class, dataset, bootstrap_size = 0.5, trees = 10) {
# For each tree, get a bootstrap subsample 
  subsamples <- replicate(trees, bootstrap_ss(dataset, bootstrap_size), 
                          simplify = FALSE)
# From each sample, learn tree
  Wtrees <- lapply(subsamples, learn_unprunned_tree, class)
# For each tree get minimum testing depth 
  depths <- lapply(Wtrees, identify_min_testing_depths)
  depths <- unlist(depths, use.names = TRUE)
  if (length(depths) == 0) stop("Only empty trees have been learned.")
# Compute weights and average across the features
  tapply(depths, names(depths), 
         function(x) sum(x ^ -0.5), simplify = TRUE) / trees
}