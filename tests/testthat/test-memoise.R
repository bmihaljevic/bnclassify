context("memoise")

test_that("memoise char nominal", {
  p <- function(x) {
    print(sample(letters, 1))
    return()
  }
  set.seed(0)
  m <- memoise_char(p)
  expect_output({invisible(m(letters)); invisible(m(letters)) }, "x")
  set.seed(0)
  m <- memoise_char(p);  
  expect_output({invisible(m("a")); invisible(m("a")) }, "x")
  m <- memoise_char(p)
  expect_error(m(NULL))
})

test_that("forget nominal", {
  p <- function(x) {
    print(sample(letters, 1))
    return()
  } 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  m <- memoise_char(p)
  expect_output({invisible(m(letters)); invisible(m(letters)) }, "x")
  forget(m)
  expect_output({invisible(m(letters)); }, "g")
})