context("memoise")

test_that("memoise char nominal", {
  p <- function(x) {
    print(sample(letters, 1))
    return()
  }
  set.seed(0)
  expect_output({m <- memoise_char(p);   invisible(m(letters)); 
                invisible(m(letters)) }, "x")
  set.seed(0)
  expect_output({m <- memoise_char(p);   invisible(m("a")); 
  invisible(m("a")) }, "x")
  m <- memoise_char(p)
  expect_error(m(NULL))
})