# Check the class is common to all dags
get_common_class <- function(x) {
  class <- unique(vapply(x, class_var, FUN.VALUE = character(1)))
  # Check it is unique
  assertthat::is.string(class)
  class
}   
ensure_list <- function(x, type = NULL) {
  if (!is_just(x, "list")) {
    x <- list(x)
  }
  if (!is.null(type)) {
    all_type <- all(vapply(x, inherits, type, FUN.VALUE = logical(1)))
    if (!all_type) stop(paste0("All elements must inherit from ", type))
  }
  x
}
# Unnamed so that it would pass no names to objects created by itearting on it
ensure_multi_list <- function(x, type = NULL) {
  unname(ensure_list(x, type = type))
}