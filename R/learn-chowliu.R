chowliu <- function(class, dataset, score='loglik', blacklist = NULL, 
                    root = NULL) {
  
  # Get pairwise scores   
  pairwise_scores <- 
    pairwise_ode_score_contribs(class = class, dataset = dataset, score = score, root = root)
  structure<-draw(pairwise_scores,root,class)
  return(structure)
}
draw<-function(pairwise_scores,root,class){
  # Get the augmenting forest   
  aug_forest <- max_weight_forest(pairwise_scores)  
  # Direct the forest (TODO: test the forest is effectively directed) 
  aug_forest <- direct_forest(aug_forest, root = root)   
  # TODO: Add blacklisting. 
  ode <- superimpose_node(dag =  aug_forest, node = class)  
  bnc_dag(dag = ode, class = class)
}

pairwise_ode_score_contribs <- function(class, dataset, score, root) {
  #   Check score in decomposable_ode_scores
  stopifnot(score %in% decomposable_ode_scores())
  # Get features   
  features<-get_features(class = class, dataset = dataset)
  
  # If 0 features then return empty graph   
  if (length(features) == 0) return(graph_empty_undirected()) 
  # If 1 feature then return single node graph (no arcs)
  pairs <- complete_graph(features)  
  if (length(features) == 1) return(pairs)  
  # Get each pair of features 
  edges <- pairs$edges  
  from <- edges[, 1]
  to <- edges[, 2]; rm(edges)
  # check dataset
  if(are_factors(dataset)){
    fun <-'local_ode_score_contrib'
    Npa <- NULL
  }
  else if(are_gaussian(dataset,class)){
    fun <-'local_ode_score_contrib_cont'
    if(score != 'loglik'){
      #construct the tan_cl structure and get its number of parameters
      pairwise_score <- mapply(fun, from, to, 
                               MoreArgs = list(class = class, dataset = dataset,param=NULL), 
                               SIMPLIFY = TRUE)
      pairwise_scores <- call_makegraph(score,pairwise_score,from,to,features)
      loglikStructure<-draw(pairwise_scores,root,class)
      #get number parameter
      classNumber <- length(levels(dataset[,class]))
      coefNumberParams <- prod(narcs(loglikStructure), classNumber)
      sdNumberParams <-prod(ncol(dataset)-1, classNumber)
      Npa <- sdNumberParams + coefNumberParams    
    }else{
      Npa <- NULL}
  }
  # For each get pairwise contribution to score
  pairwise_score <- mapply(fun, from, to, 
                           MoreArgs = list(class = class, dataset = dataset, param = Npa), 
                           SIMPLIFY = TRUE)
  stopifnot(identical(rownames(pairwise_score), decomposable_ode_scores()))
  d <- call_makegraph(score,pairwise_score,from,to,features)
  return(d)
}
call_makegraph<- function(score,pairwise_score,from,to,features){
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
local_ode_score_contrib <- function(x, y, class, dataset, param = NULL) {
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

local_ode_score_contrib_cont <- function(x, y, class, dataset, param = NULL){
  prior <- t(get_prior(dataset,class))
  classType <- as.matrix(levels(dataset[,class]))
  #compute cmi
  cor_coef <- apply(cbind(prior,classType),1,get_coef,class,x,y,dataset)
  cmi<--sum(cor_coef)/2
  #compute aic and bic
  if (!is.null(param)){
    aic <- 2*param -2*log(cmi)
    bic <- param*log(nrow(dataset)) - 2*log(cmi)
  }
  else{aic = 0
  bic = 0}
  
  c(loglik = cmi, bic = bic, aic = aic) 
  
}

get_coef <- function(var,class,x,y,dataset){
  cor_coef <- suppressWarnings( cor(subset(dataset[,y],dataset[class]==var[2]),subset(dataset[,x],dataset[class]==var[2])))
  ifelse (is.na(cor_coef),res <- 0,res <-as.numeric(var[1])*log(1-(cor_coef)^2))
  return(res)
}

