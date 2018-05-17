#' Memoise a function.
#' 
#' Based on Hadley Wickham's memoise package. Assumes that argument to f is a
#' character vector.
#' 
#' This function is a slightly modified version of
#' \code{memoise} to avoid the use of digest. The rest functions
#' copied as is from memoise.
#' 
#' @keywords internal
#' @author Hadley Wickham, Bojan Mihaljevic
#' @param f a function
memoise_char <- memoize <- function(f) {
  cache <- new_cache()
  memo_f <- function(x) {
    hash <- paste0(x, collapse = ';')
    # stopifnot(length(hash) == 1L)
    if (cache$has_key(hash)) {
      cache$get(hash)
    } else {
      res <- f(x)
      cache$set(hash, res)
      res
    }
  }
  attr(memo_f, "memoised") <- TRUE
  return(memo_f)
}
call_memoised_char <- function(x, cache) {
  do.call(cache, list(x = x))
}
#' Forget a memoized function. 
#' 
#' @keywords internal
#' @author Hadley Wickham
forget <- function(f) {
  if (!is.function(f)) return(FALSE)
  
  env <- environment(f)
  if (!exists("cache", env, inherits = FALSE)) return(FALSE)
  
  cache <- get("cache", env)
  cache$reset()
  
  TRUE
}
#' Is it memoized?
#' 
#' @keywords internal
#' @author Hadley Wickham
is.memoised <- is.memoized <- function(f) {
  identical(attr(f, "memoised"), TRUE)
}
#' Make a new cache.
#' 
#' @keywords internal
#' @author Hadley Wickham
new_cache <- function() {
  
  cache <- NULL
  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
  }
  
  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
  }
  
  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }
  
  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }
  
  cache_reset()
  list(
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = function() ls(cache)
  )
}