context("Assert")

test_that("non-empty complete Nominal ", {
  stopifnot(is_non_empty_complete(letters))
})
test_that("non-empty complete empty", {
  expect_error(check_non_empty_complete(NULL), "complete")
  expect_error(check_non_empty_complete(character()), "complete")
})  
test_that("non-empty complete 1 NA", {
    expect_error(check_non_empty_complete(c(letters, NA)), "complete")
  expect_error(check_non_empty_complete(NA), "complete")
  expect_error(check_non_empty_complete(rep(NA, 1e2)), "complete")
})
test_that("is just list",{
  expect_true(is_just(list(), "list"))
  t <- structure(list(), class="test")
  expect_true(!is_just(t, "list"))
})