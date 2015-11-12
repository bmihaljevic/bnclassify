#' Memoise.
#' @author Hadley Wickham (copied from the memoise package).
memoise <- memoize <- function(f) {
  cache <- new_cache()
  
  memo_f <- function(...) {
    hash <- digest(list(...))
    
    if (cache$has_key(hash)) {
      cache$get(hash)
    } else {
      res <- f(...)
      cache$set(hash, res)
      res
    }
  }
  attr(memo_f, "memoised") <- TRUE
  return(memo_f)
}

forget <- function(f) {
  if (!is.function(f)) return(FALSE)
  
  env <- environment(f)
  if (!exists("cache", env, inherits = FALSE)) return(FALSE)
  
  cache <- get("cache", env)
  cache$reset()
  
  TRUE
}

is.memoised <- is.memoized <- function(f) {
  identical(attr(f, "memoised"), TRUE)
}

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