# greedy_search <- function(start, move, dataset, score, epsilon, debug = FALSE) {
greedy_search <- function(class, features, start, move, dataset, epsilon, k, 
                          smooth, debug = FALSE) {
  # TODO:
  # Check start is subset of class and features. Must have class.
  # check features and class in data set .
  stopifnot(is.data.frame(dataset)) # TODO: Remove?
  stopifnot(is_positive(epsilon))
  # get all features 
  # all_features <- all features in data set
  #  Init loop variables   
  scores_log <- numeric()  
  current_dag <- NULL
  current_score <- -Inf 
  candidate_dags <- list(start)
  while (length(candidate_dags) > 0) {
    #     Score all candidate states
    scores <- dag_cv(candidate_dags, dataset, k = k, smooth = smooth)
    #     Identify the best score among candidate states 
    best_score <- max(scores)
    #     Stop if it is not better than current_score 
    better <- (best_score - epsilon >= current_score)
    if (!better) break         
    #     Identify the smallest model among best scoring states
    best_ind <- find_best_ind(scores, candidate_dags, dataset)      
    current_dag <- candidate_dags[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    #         If you have reached max score: exit (after having updated state)
    #.......if (reached_max_score(best_score, max_score)) { break }
    #     Generate all candidate from best state 
    candidate_dags <- move(state = current_dag, all_features = all_features, 
                             dataset = dataset, epsilon = epsilon, current = current_score)
  }      
#   input <- list(epsilon = epsilon, score = score)
#   state$structure <- bnc_log(state$structure, input = input, scores=scores_log)    
  current_dag
}

# if (debug) message(paste("Num. candidate states:",length(candidate_dags)))              
# if (debug) message(paste("Best candidate score ", best_score))
# if (debug) plot(current_dag$structure)      

# score_candidates <- function(candidates, dataset, k, smooth) {
#   Ensure candidates is a list 
#   Now, candidates can be a set of families.
#     So, on each fold, the learning would mean calling extract_cpt for each family
# }