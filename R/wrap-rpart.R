#' Learns a unpruned \code{rpart} recursive partition.
#' @keywords internal
learn_unprunned_tree <- function(dataset, class) {
  form <- as.formula(paste(class, '~ .'))    
  # Save time by avoiding CV or surrogates
  control <- rpart::rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                                  maxcompete = 0,maxsurrogate = 0, xval = 0)
  rpart::rpart(form, data = dataset, na.action = rpart::na.rpart, method
               = "class", parms = list(split = "information"), control = control)
}
#' Identifies all depths at which the features of a classification tree are
#' tested.
#'  
#' @param tree an \code{rpart} object
#' @return a numeric vector. The names are the names of the variables.
#' @keywords internal
identify_all_testing_depths <- function(tree) {
  stopifnot(inherits(x = tree, what = 'rpart'))
  #   Filter out leaves
  vars <- tree$frame[tree$frame$var != "<leaf>" , 'var', drop = F]
  ordering <- as.integer(rownames(vars))
  # if there are no split in the tree - return
  if (length(ordering) == 0) return(NULL) 
  names(ordering) <- as.matrix(vars)[,1]
  # Decode the depth from the ordering number. The order of a left-most child is 2*o_p, where o_p is the order of its parent.
  depths <- trunc(log(base = 2, ordering)) + 1
  # a small check
  stopifnot(min(depths) == 1)
  depths
}
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