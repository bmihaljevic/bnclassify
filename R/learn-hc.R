# Ties are resolved randomly.
greedy_search <- function(class, to_include, init, step, dataset, epsilon, k, 
                          smooth) {
  stopifnot(is_nonnegative(epsilon))
  #  Init loop variables   
  scores_log <- numeric()  
  current_dag <- NULL
  current_score <- -Inf 
  candidate_dags <- list(init)
  #   Get indices of training sets 
  test_folds <- partition_dataset(dataset, class, k)
  train <- lapply(test_folds, function(x) dataset[-x, , drop = FALSE])
  test <- lapply(test_folds, function(x) dataset[x, , drop = FALSE])
  #   Start caches for training sets 
  # TODO: smooth goes directly to cache. 
  while (length(candidate_dags) > 0) {
    # if max accuracy then break
    if (isTRUE(fast_equal(current_score, 1))) { break }
    #     Score all candidate states
    #     Update each candidate dag on the correct cache 
    #     Get the prediction for each ddag
    #     evaluate 
    scores <- cv_fixed_partition(candidate_dags, train, test, smooth = smooth)
    #     Stop if it is not better than current_score 
    if (!is_improvement(scores, current_score, epsilon)) break         
    #     Make the best dag the current one
    best_ind <- max_random(scores)
    current_dag <- candidate_dags[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    # Generate all candidates from best state 
    # ...tan_hcsp requires parameters dataset, smooth, and k; the rest do not
    candidate_dags <- step(bnc_dag = current_dag,
                           features_to_include = to_include, 
                           dataset = dataset, smooth = smooth, k = k)
  }      
  current_dag$.greedy_scores_log <- scores_log
  current_dag
}
is_improvement <- function(new_scores, current_score, epsilon) {
  (max(new_scores) - epsilon >= current_score) # TODO: Enable relative improvement
}