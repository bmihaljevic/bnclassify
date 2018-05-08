#' @export 
#' @describeIn inspect_bnc_dag  Returns the number of arcs.
narcs <- function(x) {
  num_arcs(dag(x))
}
#' Plot the structure.
#' 
#' If node labels are to small to be viewed properly, you may fix label fontsize
#' with argument fontsize. Also, you may try multiple different layouts.
#' 
#' @export
#' @inheritParams inspect_bnc_dag
#' @param y Not used
#' @param layoutType a character. Optional.
#' @param fontsize integer Font size for node labels. Optional.
#' @param ... Not used.
#' @examples  
#' 
#' # Requires the graph and Rgraphviz packages to be installed.
#' data(car)
#' nb <- nb('class', car)
#' nb <- nb('class', car)
#' \dontrun{plot(nb)}
#' \dontrun{plot(nb, fontsize = 20)}
#' \dontrun{plot(nb, layoutType = 'circo')}
#' \dontrun{plot(nb, layoutType = 'fdp')}
#' \dontrun{plot(nb, layoutType = 'osage')}
#' \dontrun{plot(nb, layoutType = 'twopi')}
#' \dontrun{plot(nb, layoutType = 'neato')}
plot.bnc_dag <- function(x, y, layoutType='dot', fontsize = NULL, ...) { 
  if (!requireNamespace("graph", quietly = TRUE)) {
    stop("Package graph needed ", call. = FALSE)
  }
  if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
    stop("Package Rgraphviz needed ", call. = FALSE)
  }
  g <- graph_internal2graph_NEL(dag(x))
  node_pars <- list(col = "green", textCol = "blue",  lty = "longdash", lwd = 1)
  if (!is.null(fontsize)) {
    node_pars$fontsize <- fontsize
  }
  l <- Rgraphviz::layoutGraph(g, layoutType = layoutType)
  Rgraphviz::renderGraph(l, graph.pars = list(nodes = node_pars))
}
#' Print basic information about a classifier.
#' @export
#' @keywords internal
print.bnc_base <- function(x, ...) {  
  cat("\n  Bayesian network classifier")  
  is_bnc_bn <- inherits(x, "bnc_bn")
  is_aode <- inherits(x, "bnc_aode")
  # says ensemble as could be others besides aode. 
  if (!is_bnc_bn & !is_aode) {
    cat(" (only structure, no parameters)")
  } 
  if (is_aode) { 
    cat(paste0("\n   An ensemble of ", nmodels(x), " Bayesian networks."))
  }
  cat("\n\n")
  cat("  class variable:       ", class_var(x), "\n")
  cat("  num. features:  ", length(features(x)), "\n")
  if (!is_aode) { 
    cat("  num. arcs:  ", narcs(x), "\n")
  }
  if (is_bnc_bn) {
    cat("  free parameters:  ", nparams(x), "\n")
  }
  if (!is.null(x$.call_struct)) {
    cat("  learning algorithm:   ", as.character(x$.call_struct[[1]]), "\n")
  }
}
#' @export 
#' @describeIn inspect_bnc_dag Returns TRUE if \code{x} is a semi-naive Bayes.
is_semi_naive <- function(x) {
  if (!is_anb(x)) return(FALSE)
  nc <- not_cci(x)
  all(vapply(nc, is_supernode, x, FUN.VALUE = logical(1)))
}
#' @export 
#' @describeIn inspect_bnc_dag Returns TRUE if \code{x} is an augmented naive Bayes.
is_anb <- function(x) {
  if (!inherits(x, 'bnc_dag')) return(FALSE)
  if (!skip_testing()) { if (!is_dag_graph(dag(x))) return(FALSE) }
  # Check call has no parents and class is in all families. This
  # code assumes class is last in each family.
  last <- unique(unlist(lapply(families(x), get_last)), use.names = FALSE)
  identical(last, class_var(x))
}
#' @export 
#' @describeIn inspect_bnc_dag Returns TRUE if \code{x} is a naive Bayes.
is_nb <- function(x) {
  is_kde(x, 0)
}
#' @export 
#' @describeIn inspect_bnc_dag Returns TRUE if \code{x} is a one-dependence estimator.
is_ode <- function(x) {
  is_kde(x, 1)
}
is_kde <- function(x, k) {
  if (!is_anb(x)) return(FALSE)
  fam_size  <- lengths(families(x), use.names = FALSE)
  max(fam_size) <= k + 2
}
# Returns sets of non class-conditionally independent features
not_cci <- function(x) {
  stopifnot(is_anb(x))
  features <- subgraph(features(x), dag(x))
  connected_components(features)
}
add_feature_parents <- function(parents, feature, x) {
  stopifnot(is_anb(x))  
  g <- condition_on(parents, feature, dag(x))
  bnc_dag(g, class_var(x))
}
# Just a convenience for calling add_feature_parents from *ply loops
add_feature_children <- function(feature, parents, x) {
  add_feature_parents(parents, feature, x)
}
relate_supernodes <- function(child_sn, parent_sn, x) {
  stopifnot(is_anb(x))  
#   check child and parent are supernodes 
  stopifnot(is_supernode(child_sn, x), is_supernode(parent_sn, x))
  g <- condition_on(parent_sn, child_sn, dag(x))
  bnc_dag(g, class_var(x))
}
add_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  a <- add_node(node, dag(x))
  class <- class_var(x)
  a <- condition_on(parents = class, nodes = node, x = a)
  bnc_dag(a, class)
}
remove_feature <- function(node, x) {
  stopifnot(assertthat::is.string(node))
  g <- remove_node(node, dag(x))
  bnc_dag(g, class_var(x))
}
is_supernode <- function(nodes, x) {
  stopifnot(!(class_var(x) %in% nodes))
  fams <- families(x)[nodes]
  arcs_within_supernode <- lapply(fams, intersect, nodes)
  n <- length(nodes)
  # Each node is counted in each family, hence additional n 
  sum(lengths(arcs_within_supernode)) ==  as.integer(n + n * (n - 1 ) / 2)
}
# Keogh
feature_orphans <- function(bnc_dag) {
  # Get the family of each feature 
  fams <- feature_families(bnc_dag)
  # Get those features whose family is of size 2 (itself and class)
  ind_orphans <- (vapply(fams, length, FUN.VALUE = integer(1)) == 2)
  fams <- fams[ind_orphans]
  if (length(fams) == 0) return(NULL)
  #  ... check they effectively are themselves and class
  feats <- features(bnc_dag)[ind_orphans]
  fams_ok <- mapply(is_orphan_fam, fams, feats, 
                    MoreArgs = list(class = class_var(bnc_dag)), 
                    SIMPLIFY = TRUE)
  stopifnot(fams_ok)                      
  # return features
  feats
} 
# Return features with up to k feature parents
# NULL if none
upto_k_parents <- function(bnc_dag, k = 0) { 
  stopifnot(all.equal(k, as.integer(k), check.attributes = FALSE), k >= 0)
  fams <- feature_families(bnc_dag)
  # Get those features whose family is of size 2 (itself and class)
  ind_orphans <- (vapply(fams, length, FUN.VALUE = integer(1)) < 2 + k)
  fams <- fams[ind_orphans] 
  if (length(fams) == 0) return(NULL)
  feats <- features(bnc_dag)[ind_orphans]
  feats 
}
is_orphan_fam <- function(fam, feat, class) {
  identical(fam, c(feat, class))
}
# ========================
# Internal
families_ids <- function(x) {
  families <- families(x)
  make_families_ids(families)
}