# greedy_search <- function(start, move, dataset, score, epsilon, debug = FALSE) {

greedy_search <- function(class, features, start, move, dataset, epsilon, k, 
                          smooth, debug = FALSE) {
  # TODO:
  # Check start is subset of class and features. Must have class.
  # check features and class in data set .
  stopifnot(is.data.frame(dataset)) # TODO: Remove?
  stopifnot(is_positive(epsilon))
  #  Init loop variables   
  scores_log <- numeric()  
  current_state <- NULL
  current_score <- -Inf 
  candidate_states <- list(start)
  while (length(candidate_states) > 0) {
    if (debug) message(paste("Num. candidate states:",length(candidate_states)))                
    #     Score all candidate states
    scores <- score_candidates(candidate_states, dataset, k = k, smooth = smooth)
    #     Identify the best score among candidate states 
    best_score <- max(scores)
    if (debug) message(paste("Best candidate score ", best_score))
    #     If it is not better than current_score: 
    better <- (best_score - epsilon >= current_score)
    if (!better) break         
    #     Identify the smallest model among best scoring states
    best_ind <- find_best_ind(scores, candidate_states, dataset)      
    current_state <- candidate_states[[best_ind]]
    current_score <- scores[[best_ind]]
    scores_log <- c(scores_log, current_score)
    if (debug) plot(current_state$structure)      
    #         If you have reached max score: exit (after having updated state)
    #.......if (reached_max_score(best_score, max_score)) { break }
    #     Generate all candidate from best state 
    candidate_states <- move(state = current_state, dataset = dataset, 
                             epsilon = epsilon, current = current_score)
  }      
  input <- list(epsilon = epsilon, score = score)
  state$structure <- bnc_log(state$structure, input = input, scores=scores_log)    
  state
}
# inherits_just(x, class) {
#   identical(class(x), class)
# }
# score_candidates <- function(candidates, dataset, k, smooth) {
#   Ensure candidates is a list 
#   Now, candidates can be a set of families.
#     So, on each fold, the learning would mean calling extract_cpt for each family
# }
# 
# include_node
# is_semi_naive
# features (nodes - 1)
# 
# 
# These functions operate on this higher level, the bnc_dag. There is no need to operate on that level, the functions can take the class and the graphNELL.
# 
# The reason to avoid this level is that the objects are more expensive to create. I could create a mini bnc_dag, which contains just the dag and the class. The rest is hardly useful. However, some of that, like the families and so on, might be useful if operations are repeated. 
# 
# Another advantange is that modifying a graph probably does not make a copy! Thus, working on ti will be much faster. And efficient. Working on an object if mine would be OK if it had reference semantics. 
# 
# Instead of operating with graphs, I suppose it would be best to have a separate structure to operate on, and validate it from time to time to check whether it is correct. 
# 
# NEL. Changing a NEL maybe does not change all of its components. 
# Yet, I could have a smaller NEL that would not even copy the very list that is updated but just modify in place. 

# is_semi_naive
#   checks for completeness of subgraphs.
#   checks for augmented naive bayes, which looks at the parents of each.
# include_node
# related_feature_subsets
# relate_nodes


# The CPTs format is one of incoming edges; the dag edgeL is outgoing. Different. 
# Yet, I probably can implement most operations for the family format. 
#   feature_parents(): very simple. 
#   related_feature_subsets(): supernodes(): here i may convert to graph.......
#     This I must resolve. 
#     supernodes() is a better name. 
#   condition_to(): trivial. 
#   to_graphNEL(): make a from-to matrix from my object.
#   relate_nodes():
#       form_supernode() is a better name 
#       repeatedly call condition_to().
#   is_dag(): could use gRbase.    
#       
# My format will probably not be very good for traversing but it will be for modifying.      
#   
# So, I will use a bnc_dag here, because I need to know the class and some other methods,
# but I may construct a light version. 
# 
# include_node should call 'condition_to()'
# 
# include_by_join <- function(bnc_dag, all_features) {
#   stopifnot(is_semi_naive(dag))
#   if (length(features(model)) == 0) return(NULL)  
#   l <- lapply(all_features, function(node) {
#     augmented <- include_node(node, x = model)
#     stopifnot(is_semi_naive(augmented))
#     #     get them from model (not augmented) so it does not contain 'node'
#     related_features <- related_feature_subsets(x = model)    
#     structs <- lapply(related_features, relate_nodes, node, augmented)
#     stopifnot(all(sapply(structs, is_semi_naive)))
#     potential_nodes <- setdiff(state$potential_nodes, node)
#     lapply(structs, search_state, potential_nodes)
#   })
#   # Do I need names? With is use.names = FALSE it is faster.
#   unlist(l, recursive = FALSE)
# }


# bnc_cv(x, lp) {
#   dags <- NULL
#   X is a list of calls
#     Update each x to use dataset
#     dags <- call each x
#   X is a list of dags
#     dags <- x
#     For each x, update lp to use x and dataset
#   X is empty 
#     Nothing
#   Return the result of lp  
# }
# Returns a call to lp that reproduce all steps of the learning. 
# Handles the options of combining structures and dag learning with param learning. 
# 
# bnc_updateable(nbcar, struct = FALSE) {
#   if struct, return both calls
#   if not struct, return just lpcall
# }
# 
# # uncover_learning(nbcar, struct = FALSE)
# #   Returns the underlying lp call. 
# #     Then I have to change its argument to use a new dag. 
# # uncover_learning(nbcar, struct = TRUE)
# #   Returns the underlying lpcall with the dag call inserted in it.
# #     Maybe they can be merged elsewhere
#   
# # Test this from a dummy function:    
#     
# bnc_updateable(dag, lpcall) {
#   If dag is dag, put it in lpcall. 
#   If dag is call, call it and put its result in lpcall. 
#   Return lpcall
# }
# 
# # Updates all relevant parts of the lp function to use the new data set
# bnc_update_ds() {
#   
# }
# 
# bnc_cv(lpcall, dataset, k) {
#   List of lpcalls. If not, the call bnc_updateable on it.
#   Call cross val. That will simply run the call on the subset. 
# }
# 
# 
# tanhc(class, dataset, lp) {
#   x <- candidates()
#   lapply(x, bnc_updateable, lp = lp)
# }
# 
# tanhc <- function(class, dataset, lp) {
#   candidates <- step(x, dataset)
#   lapply(candidates, modify_call)
#   pryr::modify_call(lp, list())
#   bnc_cv(x, lp)
#   
# }
# 
# tanhc <- function(class, dataset, lp) {
# #   lp must be a call
#   dag <- nb(class, dataset)
#   lpcall <- substitute(lp)
#   wdataset <- dataset[1:2, ]
#   lpcall <- pryr::modify_call(lpcall, list(x=substitute(dag), 
#                                            dataset = substitute(wdataset)))
#   eval(lpcall, parent.frame())
# #   must pass it to the cv function, i.e, to model updating
# #   this means to update its parameter learning on a subset of data. 
# # If I want to use bnc_update, there must be a call to bnc(). Otherwise, no point.
# # bnc_update(list(.call = list(lp = substitute(lp))), dataset[1:2, ])
# }
#