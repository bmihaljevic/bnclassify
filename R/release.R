release_questions <- function() {
  c(
    "Have you checked all TODOs in code?",
    "Have you removed all commmented-out, not used code? Also in tests.",
    "Have you checked on Travis?",
    "Does cran_comments.md show the notes from R-devel?",
    "Does cran_comments.md specify the correct versions of R where tested?"
  )
}