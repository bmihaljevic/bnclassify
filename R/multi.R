# Check the class is common to all dags
get_common_class <- function(x) {
  class <- unique(vapply(x, class_var, FUN.VALUE = character(1)))
  # Check it is unique
  assertthat::is.string(class)
  class
}
get_common_cp <- function(x, just_first = FALSE) {
  # Assuming the cp is identical in each of them.
  # Check it. 
  # Check is a boolean. Can skip it. 
  # If not check then return just first
}
extract_unique_cpts <- function(x, dataset, smooth) {
  ufams <- unique_families(lapply(x, families))
  families2cpts(ufams, dataset = dataset, smooth = smooth)
}
ensure_list <- function(x) {
  if (!is_just(x, "list")) {
    x <- list(x)
  }
  x
}