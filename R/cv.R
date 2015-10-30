# Makes stratified cross-validation folds.
make_cv_test_folds <- function(dataset, class, k) {
  # Check data dim. check k integer. check class a character?
  Y <- dataset[[class]]
  ygrouped = crossval:::group.samples(Y)
  groupsize = sapply(ygrouped, length)
  nfolds = min(k, max(groupsize))
  crossval:::get.folds(ygrouped, K = nfolds)
}
cv_fixed_partition <- function(x, train, test, smooth) {
  # need > 1 because if result is 1 dimensional I assume that there is only one x 
  stopifnot(length(train) > 1, length(train) == length(test))
  p <- mapply(learn_and_assess, train, test, 
              MoreArgs = list(x = x, smooth = smooth), SIMPLIFY = TRUE)
  if (is.null(dim(p)) || length(dim(p)) < 2) {
    p <- t(p)
  }
  rowMeans(p)
}
learn_and_assess <- function(train, test, x, smooth) {
  x <- ensure_multi_list(x)
  class <- get_common_class(x)
  x <- lapply(x, lp, dataset = train, smooth = smooth)
  predictions <- lapply(x, predict, test,  prob = FALSE)
  vapply(predictions, accuracy, test[, class], FUN.VALUE = numeric(1))
}