multi_crossval <- function(x, dataset, k, dag, smooth) {
  x <- ensure_multi_list(x)
  class <- get_common_class(x)
  # Prepare data for crossval::crossval
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  ind_class <- which(colnames(dataset) == class)
  cv_results <- 
    crossval::crossval(crossval_callback, dataset[, -ind_class, drop = FALSE], 
                       dataset[, ind_class], K = k, B = 1, verbose = FALSE,
                       x = x, dag = dag, smooth = smooth, class = class)
  cv_results$stat
}
# Do a cross validation instance with the cross val package
crossval_callback <- function(train.x, train.y, test.x, test.y, x, dag, smooth, 
                              class) {
  dataset <- cbind(train.x, class = train.y) 
  names(dataset)[ncol(dataset)] <- class
  ux <- multi_update(x, dataset, dag = dag, smooth = smooth)
  # Predict for each x 
  predictions <- multi_predict(ux, test.x, prob = FALSE)
  # Compute accuracy
  vapply(predictions, accuracy, test.y, FUN.VALUE = numeric(1))
}