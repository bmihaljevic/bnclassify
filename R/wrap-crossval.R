# Runs cross-validation
do_crossval <- function(x, class, dataset, k) {
  # Find the class column
  cnames <- colnames(dataset)
  stopifnot(!are_disjoint(class, cnames))
  ind_class <- which(colnames(dataset) == class)
  # Call cross-validation
  cv_results <- 
    crossval::crossval(crossval_fun, dataset[, -ind_class, drop = FALSE], 
                       dataset[, ind_class], K = k, B = 1, verbose = FALSE, x=x)
  cv_results$stat
}
# Do a cross validation instance with the cross val package
crossval_fun <- function(train.x, train.y, test.x, test.y, x) {
  # Check x is a plain list 
  stopifnot(inherits(x, "list")) 
  dataset <- cbind(train.x, class=train.y) 
  # Update each x 
  updated_x <- lapply(x, bnc_update, dataset = dataset)
  # Predict for each x 
  predictions <- lapply(updated_x, predict, test.x)
  # Compute accuracy
  vapply(predictions, accuracy, test.y, FUN.VALUE = numeric(1))
}