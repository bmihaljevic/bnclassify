context("Miscallaneous")

test_that("make_last", {
#   Nominal
  e <- make_last(letters, 'c')
  expect_equal(e, c(letters[-3], letters[3]))
#   x not character
  expect_error(make_last(1:10, 'c')  , "character")
# last not in x  
  expect_error(make_last(letters, 'A'), "length")
# last repeated in x  
  expect_error(make_last(rep('A', 5), 'A'), "length")
})

test_that("Rep factor as int", {
  fi <- rep_factor_as_int(factor(letters, levels = letters), 10)
  expect_identical(length(fi), 260L)
  expect_true(all(fi == 1:26))
})