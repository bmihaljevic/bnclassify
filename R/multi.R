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
  x <- ensure_list(x)
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
# compute_lucp_multi <- function(x, dataset)  {
#   # We can only apply the optimization if data is complete 
#   if (!anyNA(dataset)) {
#     compute_lucp_multi_complete(x, dataset)
#   }
#   else {
#     stop("Not implemented.")
#   }
# }
# 
# compute_lucp_multi_complete <- function(x, dataset) {
#   stopifnot(!anyNA(dataset)) 
#   compute_augnb_lucp_multi(x, dataset)
# }