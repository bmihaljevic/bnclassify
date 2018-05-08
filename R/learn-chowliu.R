chowliu <- function(class, dataset, score='loglik', blacklist = NULL, 
                    root = NULL) {
# Get pairwise scores   
  pairwise_scores <- 
    pairwise_ode_score_contribs(class = class, dataset = dataset, score = score)
# Get the augmenting forest   
  aug_forest <- max_weight_forest(pairwise_scores)  
# Direct the forest (TODO: test the forest is effectively directed) 
  aug_forest <- direct_forest(aug_forest, root = root)    
# TODO: Add blacklisting. 
  ode <- superimpose_node(dag =  aug_forest, node = class)  
  bnc_dag(dag = ode, class = class)
}
pairwise_ode_score_contribs <- function(class, dataset, score) {
#   Check score in decomposable_ode_scores
  stopifnot(score %in% decomposable_ode_scores())
# Get features   
  features = get_features(class = class, dataset = dataset)
# If 0 features then return empty graph   
  if (length(features) == 0) return(graph_empty_undirected()) 
# If 1 feature then return single node graph (no arcs)
  pairs <- complete_graph(features)  
  if (length(features) == 1) return(pairs)  
# Get each pair of features 
  edges <- pairs$edges  
  from <- edges[, 1]
  to <- edges[, 2]; rm(edges)
# For each get pairwise contribution to score
  pairwise_score <- mapply(local_ode_score_contrib, from, to, 
                     MoreArgs = list(class = class, dataset = dataset), 
                     SIMPLIFY = TRUE)
  stopifnot(identical(rownames(pairwise_score), decomposable_ode_scores()))
# Select the score 
  pairwise_score <- pairwise_score[score, ]  
# Remove negative scores (possible for BIC and AIC) and weight the edges
# ...If I also removed 0 scores --which are possible for loglik-- the structure
# ...may turn out a forest even when using loglik
  ind_keep <- pairwise_score >= 0
  from <- from[ind_keep]
  to <- to[ind_keep]
  pairwise_score <- pairwise_score[ind_keep]
  make_graph(features, from, to, pairwise_score)
}
#' Returns pairwise component of ODE (penalized) log-likelihood scores. 
#' In natural logarithms.  
#' @keywords internal
local_ode_score_contrib <- function(x, y, class, dataset) {  
  #   If x and y and class do not have length one stop
  stopifnot(length(x) == 1)
  stopifnot(length(y) == 1)
  stopifnot(length(class) == 1)  
#  Get contingency table  
  freqs <- extract_ctgt(c(x, y, class), dataset)  
# Ignore dataset from here on
  rm(dataset)
#  Compute I(X;Y | Z) 
  cmi <- cmi_table(freqs, unit = "log")
#   Get number of degrees of freedom
  df <- cmi_degrees_freedom(freqs_table = freqs)  
#  Make sure it is non-negative 
  stopifnot(df >= 0)
#  Get num. of observations in contingency table
  N <- sum(freqs)
#  Compute bic
  bic <- N * cmi  - (log(N) / 2) * df
#  Compute aic 
  aic <- N * cmi  - df 
  c(loglik = cmi, bic = bic, aic = aic)  
}
decomposable_ode_scores <- function() { c('loglik', 'bic', 'aic') }