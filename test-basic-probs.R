context("basic probs")

make_factor <- function(n) {
  a <- runif(100)
  b <- 1 - a 
  td <- matrix(c(a, b), ncol=2)
  colnames(td) <- letters[1:2]
  td
}

test_that("Multiply single factor", {
  td <- make_factor(1000)
  a <- sum_matrices(list(a=td))
  expect_equal(td, a)
})  

test_that("Multiply Two factors", {
  td <- make_factor(1000)
  td2 <- make_factor(1000)
  a <- sum_matrices(list(a=td, b=td2))
  expect_identical(colnames(a), letters[1:2])
  expect_identical(dim(td), dim(a))
  fr <- td[1, ] + td2[1, ]
  expect_equal(fr, a[1, ])
})


test_that("normalize", {
  # Nominal 
  a <- setNames(1:5, letters[1:5])
  a <- normalize(a)
  expect_equal(sum(a), 1)
  expect_equal(length(a), 5)
  expect_equal(unname(a), 1:5 / sum(1:5))
  expect_equal(names(a), letters[1:5])
  # Sum 0 
  a <- normalize(setNames(rep(0, 10), letters[1:10]))
  expect_equal(sum(a), 1)
  expect_equal(length(a), 10)
  expect_equal(unname(a), rep(1 / 10, 10))
  expect_equal(names(a), letters[1:10])
  # Single 0. Degenerate prob. dist.
  a <- normalize(0)
  expect_equal(a, 1)
})

test_that("Legal prob. distribution", {
  expect_true(are_pdists(matrix(1)))
  expect_true(!are_pdists(matrix(0)))
  expect_true(!are_pdists(matrix(NA, 1)))
  expect_true(!are_pdists(matrix(c(-1, 1), nrow=1, byrow = TRUE)))
  expect_true(are_pdists(matrix(1:5 / 15, nrow=1, byrow = TRUE)))
})