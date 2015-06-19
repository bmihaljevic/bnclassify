# Check the class is common to all dags
get_common_class <- function(x) {
  class <- unique(vapply(x, class_var, FUN.VALUE = character(1)))
  # Check it is unique
  assertthat::is.string(class)
  class
}
# Unique in terms of the variables, I assume their contents are identical if
# their vars match
get_unique_cpts <- function(x) {
  x <- ensure_multi_list(x)
  ufams <- unique_families(lapply(x, families))
  ufams_ids <- as.vector(make_families_ids(ufams))
  all_cpts <- unlist(lapply(x, params), recursive = FALSE)
  all_cpts_ids <- vapply(lapply(all_cpts, cpt2family), make_family_id, 
                         FUN.VALUE = character(1))
  all_cpts[match(ufams_ids, all_cpts_ids)]
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
# Unnamed so that it would pass no names to objects created by itearting on it
ensure_multi_list <- function(x) {
  unname(ensure_list(x))
}