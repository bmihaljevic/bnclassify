# greedy_search <- function(init, step, dataset, score, epsilon, debug = FALSE) {
greedy_search <- function(class, to_include, init, step, dataset, epsilon, k, 
                          smooth) {
  # TODO: # Check init is subset of class and features. Must have class.
  # check features and class in data set .
  stopifnot(is_nonnegative(epsilon))
  # get all features 
  # all_features <- all features in data set
  #  Init loop variables   
  scores_log <- numeric()  
  current_dag <- NULL
  current_score <- -Inf 
  candidate_dags <- list(init)
  while (length(candidate_dags) > 0) {
    #     Score all candidate states
    scores <- dag_cv(candidate_dags, class = class, dataset = dataset, 
                     smooth = smooth, k = k)
    #     Identify the best score among candidate states 
    best_score <- max(scores)
    #     Stop if it is not better than current_score 
    better <- (best_score - epsilon >= current_score) # Refactor. 
    if (!better) break         
    best_ind <- which.max(scores) # TODO: change to random or min.
    current_dag <- candidate_dags[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    #         If you have reached max score: exit (after having updated state)
    #.......if (reached_max_score(best_score, max_score)) { break }
    #     Generate all candidate from best state 
    candidate_dags <- step(bnc_dag = current_dag,
                           features_to_include = to_include)
#     dataset = dataset, epsilon = epsilon, 
#     current = current_score
  }      
  list(model = current_dag, log = scores_log)
}

#     Identify the smallest model among best scoring states
# best_ind <- find_best_ind(scores, candidate_dags, dataset)      

# , debug = FALSE

#   input <- list(epsilon = epsilon, score = score)
#   state$structure <- bnc_log(state$structure, input = input, scores=scores_log)
# if (debug) message(paste("Num. candidate states:",length(candidate_dags)))              
# if (debug) message(paste("Best candidate score ", best_score))
# if (debug) plot(current_dag$structure)      

# score_candidates <- function(candidates, dataset, k, smooth) {
#   Ensure candidates is a list 
#   Now, candidates can be a set of families.
#     So, on each fold, the learning would mean calling extract_cpt for each family
# }