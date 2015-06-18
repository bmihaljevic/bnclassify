context("Multi") 

test_that("Multi bnc bn single dag", {
  n <- nb('class', car)
  nb <- bnc_bn(n, car, smooth = 0.1, call = NULL)
  nm <- multi_bnc_bn(n, car, smooth = 0.1, call = NULL)
  expect_identical(nb, nm[[1]])
})

test_that("Multi bnc bn two dags", {
  n <- nb('class', car)
  nb <- bnc_bn(n, car, smooth = 0.1, call = NULL)
  nm <- multi_bnc_bn(list(n, n), car, smooth = 0.1, call = NULL)
  expect_identical(nb, nm[[1]])
  expect_identical(nb, nm[[2]])
})

test_that("Multi bnc bn different class vars dags", {
  n <- nb('class', car)
  v <- nb('Class', voting)
  expect_error(multi_bnc_bn(list(n, v), car, smooth = 0.1, call = NULL),
               "string")
})

test_that("unique CPTs nominal", {
  e <- nbcarp(car[, 5:7])
  d <- nbcarp(car[, c(1:3, 7)])
  d <- extract_unique_cpts(list(e, d), car, smooth = 1)
  expect_equal(length(d), 6)
  expect_true(is_perm(names(d), colnames(car)[-4]))
})