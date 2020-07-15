context("Miscallaneous")

test_that("make_last", {
#   Nominal
  # so to not modify letters
  e <- c(letters, NULL)
  e <- make_last_sideeffect(e, 'c')
  expect_equal(e, c(letters[-3], letters[3]))
# #   x not character
  # Currently not checking this
#   expect_error(make_last(1:10, 'c')  , "character")
# last not in x  
  e <- c(letters, NULL)
  expect_error(make_last_sideeffect(e, 'A'), "not found")
# last repeated in x  
  # TODO: does not report a mistake.
  # expect_error(make_last_sideeffect(rep('A', 5), 'A'), "length")
})

test_that("Rep factor as int", {
  fi <- rep_factor_as_int(factor(letters, levels = letters), 10)
  expect_identical(length(fi), 260L)
  expect_true(all(fi == 1:26))
})

test_that("Random max nominal", { 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  x <- c(1, runif(5), 1)
  a <- max_random(x)
  b <- max_random(x)
  expect_true(a != b)
  expect_equal(x[a], x[b])
})

test_that("Boostrap nominal", {
  d <- bootstrap_ss(dataset = car, proportion = 0.25)  
  expect_equal(dim(d), c(432, ncol(car)))
  expect_equal(colnames(d), colnames(car))
  
  d <- bootstrap_ss(dataset = voting, proportion = 0.2)  
  expect_equal(dim(d), c(87, 17))
  expect_equal(colnames(d), colnames(voting))  
})

test_that("Boostrap 0 proportion", {
  expect_error(bootstrap_ss(dataset = car, proportion = 0), "positive")  
})