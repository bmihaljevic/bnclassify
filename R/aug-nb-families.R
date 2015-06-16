# Returns bnc_families but with a unique name
tag_families <- function(fams) {
  ids <- lapply(fams, paste0, collapse = "")
  setNames(fams, nm = ids)
}
# Returns the unique families accross these dags. 
unique_families <- function(fams) {
  all_fams <- unlist(fams, recursive = FALSE)
  # Does not take into account that same may only differ in the order of parents
  # Use duplicated instead of unique in order to preserve the names
  all_fams[!duplicated(all_fams)]
}
# # Ensure x is a list
# if (!is_just(x, "list")) {
#   x <- list(x)
# }
# fams <- lapply(x, bnc_families)