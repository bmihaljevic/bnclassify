multi_crossval <- function(x, dataset, k, dag) {
  x <- ensure_list(x)
  class <- get_common_class(x)
  # Prepare data for crossval::crossval
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  ind_class <- which(colnames(dataset) == class)
  cv_results <- 
    crossval::crossval(crossval_callback, dataset[, -ind_class, drop = FALSE], 
                       dataset[, ind_class], K = k, B = 1, verbose = FALSE,
                       x = x, dag = dag, class = class)
  cv_results$stat
}
# Do a cross validation instance with the cross val package
crossval_callback <- function(train.x, train.y, test.x, test.y, x, dag, class) {
  dataset <- cbind(train.x, class = train.y) 
  names(dataset)[ncol(dataset)] <- class
  ux <- multi_update(x, dataset, dag = dag)
  # Predict for each x 
  predictions <- multi_predict(ux, test.x, prob = FALSE)
  # Compute accuracy
  vapply(predictions, accuracy, test.y, FUN.VALUE = numeric(1))
}


# Runs cross-validation
do_crossval <- function(x, class, dataset, k) {
  # Find the class column
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  ind_class <- which(colnames(dataset) == class)
  # Call cross-validation
  cv_results <- 
    crossval::crossval(crossval_fun, dataset[, -ind_class, drop = FALSE], 
                       dataset[, ind_class], K = k, B = 1, verbose = FALSE, x=x,
                       class = class)
  cv_results$stat
}
# Do a cross validation instance with the cross val package
crossval_fun <- function(train.x, train.y, test.x, test.y, x, class) {
  # Check x is a plain list 
  stopifnot(inherits(x, "list")) 
  dataset <- cbind(train.x, class = train.y) 
  names(dataset)[ncol(dataset)] <- class
  # Update each x 
  updated_x <- lapply(x, bnc_update, dataset = dataset)
  # Predict for each x 
  predictions <- lapply(updated_x, predict, test.x)
  # Compute accuracy
  vapply(predictions, accuracy, test.y, FUN.VALUE = numeric(1))
}
do_crossval_multi <- function(x, class, dataset, smooth, k) {
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  ind_class <- which(colnames(dataset) == class)
  # Call cross-validation
  cv_results <- 
    crossval::crossval(crossval_fun_multi, dataset[, -ind_class, drop = FALSE], 
                       dataset[, ind_class], K = k, B = 1, verbose = FALSE, x = x,
                       smooth = smooth)
  cv_results$stat
}
crossval_fun_multi <- function(train.x, train.y, test.x, test.y, x, smooth) {
  # Check x is a plain list 
  stopifnot(inherits(x, "list")) 
  dataset <- cbind(train.x, class = train.y) 
  predictions <- multi_learn_predict(x, train = dataset, test = test.x, 
                                     smooth = smooth, prob = FALSE)
  # Compute accuracy
  vapply(predictions, accuracy, test.y, FUN.VALUE = numeric(1))
}