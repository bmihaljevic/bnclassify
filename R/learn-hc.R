# Ties are resolved randomly.
greedy_search <- function(class, to_include, init, step, dataset, epsilon, k, 
                          smooth, cache_reset = NULL) {
  stopifnot(is_nonnegative(epsilon))
  #  Init loop variables   
  scores_log <- numeric()  
  current_dag <- NULL
  current_score <- -Inf 
  candidate_dags <- list(init)
  #   Get indices of training sets 
  test_folds <- partition_dataset(dataset, class, k)
  train <- lapply(test_folds, function(x) dataset[-x, , drop = FALSE])
  train <- lapply(train, make_cpts_cache, smooth = smooth)
  test <- lapply(test_folds, function(x) dataset[x, , drop = FALSE])
  test <- lapply(test, make_evidence)
  #   Start caches for training sets 
  # TODO: smooth goes directly to cache. 
  # skip asserts during greedy search
  skip_env$skip_assert <- TRUE
  while (length(candidate_dags) > 0) {
    # if max accuracy then break
    if (isTRUE(fast_equal(current_score, 1))) { break }
    #     Score all candidate states
    #     Update each candidate dag on the correct cache 
    #     Get the prediction for each ddag
    #     evaluate 
    scores <- cv_lp_partition(candidate_dags, train, test)
    #     Stop if it is not better than current_score 
    if (!is_improvement(scores, current_score, epsilon)) break         
    #     Make the best dag the current one
    best_ind <- max_random(scores)
    current_dag <- candidate_dags[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    if (!is.null(cache_reset)) {
      if (length(scores_log) %% cache_reset == 0) lapply(train, forget)
    }
    # Generate all candidates from best state 
    # ...tan_hcsp requires parameters dataset, smooth, and k; the rest do not
    candidate_dags <- step(bnc_dag = current_dag,
                           features_to_include = to_include, 
                           train = train, test = test)
  }      
  # Turn assert back on  
  skip_env$skip_assert <- FALSE
  current_dag$.greedy_scores_log <- scores_log
  current_dag
}

# Ties are resolved randomly.
greedy_search_scores <- function(class, to_include, init, step, dataset, score, 
                                 cache_reset = NULL) {
  #  Init loop variables   
  scores_log <- numeric()  
  current_dag <- NULL
  current_score <- -Inf 
  candidate_dags <- list(init)
  # TODO: smooth goes directly to cache. 
  # skip asserts during greedy search
  skip_env$skip_assert <- TRUE
  while (length(candidate_dags) > 0) {
    # if max accuracy then break
    if (isTRUE(fast_equal(current_score, 1))) { break }
    #     Score all candidate states
    #     Update each candidate dag on the correct cache 
    #     Get the prediction for each ddag
    #     evaluate 
    
    #scores <- get_scores(candidate_dags, class, dataset, score)
    scores <- mapply(get_scores,candidate_dags, 
                     MoreArgs = list(class=class, dataset=dataset, score=score), SIMPLIFY = TRUE)
    
    #     Stop if it is not better than current_score 
    if (!is_improvement(scores, current_score, 0)) break         
    #     Make the best dag the current one
    best_ind <- max_random(scores)
    current_dag <- candidate_dags[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    # Generate all candidates from best state 
    # ...tan_hcsp requires parameters dataset, smooth, and k; the rest do not
    candidate_dags <- step(bnc_dag = current_dag,
                           features_to_include = to_include)
  }      
  # Turn assert back on  
  skip_env$skip_assert <- FALSE
  current_dag$.greedy_scores_log <- scores_log
  current_dag
}

get_scores<- function(candidate_dag, class, dataset, score){
  edges <-candidate_dag$.dag$edges
  from <- edges[, 1]
  to <- edges[, 2]
  
  x<-from!=class
  father<-from[x]
  a<- to %in% to[x]
  index<-which(a,TRUE)
  nw_from <- list()
  nw_to<-list()
  y<-1
  for (i in 1:length(from)){
    if(from[i]==class & i %in% index){
      nw_from[[i]]<- c(from[i], father[y])
      nw_to[i]<- to[i]
      y<-y+1
    }
    else if(from[i]==class){
      nw_from[i]<- from[i]
      nw_to[i]<- to[i]
    }
  } 
  
  #pairwise_score <- family_scores(nw_from[[1]], nw_to[[1]], dataset, score) 
  
  pairwise_score <- mapply(family_scores, nw_from, nw_to, 
                          MoreArgs = list(dataset = dataset, score=score), 
                          SIMPLIFY = TRUE)
  ##sum
  sum(pairwise_score)
}

family_scores <- function(x, y, dataset, score){
  N<-nrow(dataset)
  freqs <- extract_ctgt(c(x,y), dataset)  
  unit<-"log"
  if(score=='loglik'){ 
    entrpy <- entropy::entropy(freqs, method = "ML", unit = unit, verbose = F)
    mi<-mutual_information(x, y, dataset)
    return(N * (mi - entrpy))}
  if(score=='bic'){
    entrpy <- entropy::entropy(freqs, method = "ML", unit = unit, verbose = F)
    df<-cmi_degrees_freedom(freqs)
    return (N * (mutual_information(x, y, dataset) - entrpy) - log(N) / 2 * df)}
  if(score=='aic'){
    entrpy <- entropy::entropy(freqs, method = "ML", unit = unit, verbose = F)
    df<-cmi_degrees_freedom(freqs)
    return (N * (mutual_information(x, y, dataset) - entrpy) - df)}
}


mutual_information<-function(x, y, dataset){
  total<-nrow(dataset)
  MI<-0
  freq_table_child<-as.data.frame(table(dataset[[y]]))
  t<- table(dataset[,x])
  freq_table_father<-as.data.frame(t)
  freq_table_father<-freq_table_father[freq_table_father["Freq"] !=0,]
  for( row_c in 1:nrow(freq_table_child)){
    child<-freq_table_child[row_c,]
    pa<-child[["Freq"]]/total
    for( row_f in 1:nrow(freq_table_father)){
      father<-freq_table_father[row_f,]
      pb<- father[["Freq"]]/total
      numerator<-dataset[dataset[y] == as.vector(child[["Var1"]]),] ##& dataset[y] == as.vector(father[["Var1"]])
      for (i in 1:length(x)){
        if (nrow(numerator)!=0){
          numerator<-numerator[numerator[x[i]] == as.vector(father[[i]]),]
        }
      }
      numerator <- nrow(numerator)
      pa_b<- numerator/total
      if (pa_b!=0){ MI<-MI + (pa_b* log2(pa_b/(pa*pb)))}
    }
  }
  MI
}


is_improvement <- function(new_scores, current_score, epsilon) {
  (max(new_scores) - epsilon >= current_score) # TODO: Enable relative improvement
}