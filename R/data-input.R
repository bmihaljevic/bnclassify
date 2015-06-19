# Checks it is a data frame, with all named unique factor columns.
check_dataset <- function(dataset) {  
#   Check dataset is a data frame  
  stopifnot(is.data.frame(dataset))  
#   Check every column has a unique name 
  cnames <- colnames(dataset)
  stopifnot(is_non_empty_complete(cnames), are_all_unique(cnames))
#   Make sure they are all factors 
  stopifnot(are_factors(dataset))
}
# Checks class is a length 1 character
check_class <- function(class) { 	
#   Check class is length 1 character   
  stopifnot(assertthat::is.string(class))
} 
# Check the features are in correct format
check_features <- function(features, class) {
  # Check class 
  check_class(class)
  # Check features are NULL or character 
  stopifnot(is.null(features) || is.character(features))
  # Check class is not in features
  stopifnot(are_disjoint(class, features))
 }
# Checks class is a length 1 character found in the dataset
check_class_in_dataset <- function(class, dataset) {
  check_class(class)
  #   Check data set has unique column names
  check_dataset(dataset)  
  #   Check class in dataset
  stopifnot(!are_disjoint(class, colnames(dataset)))  
}
# Gets the features in a data set
get_features <- function(class, dataset) {
#   Check class 
  check_class_in_dataset(class, dataset)
#   Return all column names other than class  
  setdiff(colnames(dataset), class)
}
# Trim the dataset to the vars 
trim_dataset <- function(vars, dataset) {
  # Check dataset
  check_dataset(dataset)
  # Check vars are unique and non empty 
  stopifnot(is_non_empty_complete(vars))
  stopifnot(is.character(vars))
  stopifnot(are_all_unique(vars))
  # If the same return original dataset
  if (is_perm(vars, colnames(dataset))) {
    dataset
  }
  # Otherwise trim it
  else {
    dataset[ , vars, drop=FALSE]
  }
}