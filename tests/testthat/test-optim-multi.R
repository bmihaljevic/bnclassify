context("Multi") 

test_that("unique CPTs nominal", {
  e <- nbcarp(car[, 5:7])
  d <- nbcarp(car[, c(1:3, 7)])
  d <- extract_unique_cpts(list(e, d), car, smooth = 1)
  expect_equal(length(d), 6)
  expect_true(is_perm(names(d), colnames(car)[-4]))
})

test_that("get unique cpts nominal" ,{
  a <- nbcarp(car[, 5:7])
  b <- nbcarp(car[, c(1:3, 7)])
  c <- get_unique_cpts(list(a, b))
  expect_equal(c[1:3], params(a))
  expect_equal(c[4:6], params(b)[-4])
})

test_that("get unique cpts single dag" ,{
  a <- nbcarp(car[, 5:7])
  b <- get_unique_cpts(a)
  expect_identical(params(a), b)
})