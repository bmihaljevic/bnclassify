context("HC")

test_that("nominal", {
  a <- tanhc('class', car, lp = lp(smooth = 1))
  bnc_params(a)[[2]]
})