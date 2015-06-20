context("Multi") 

test_that("Multi bnc bn single dag", {
  n <- nb('class', car) 
  nb <- bnc_bn(n, car, smooth = 0.1)
  nm <- multi_bnc_bn(n, car, smooth = 0.1)
  expect_identical(nb, nm[[1]])
})

test_that("Multi bnc bn two dags", {
  n <- nb('class', car)
  nb <- bnc_bn(n, car, smooth = 0.1)
  nm <- multi_bnc_bn(list(n, n), car, smooth = 0.1)
  expect_identical(nb, nm[[1]])
  expect_identical(nb, nm[[2]])
})

test_that("Multi bnc bn different class vars dags", {
  n <- nb('class', car)
  v <- nb('Class', voting)
  expect_error(multi_bnc_bn(list(n, v), car, smooth = 0.1),
               "string")
})

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

test_that("learn and predict", {
  a <- nb('class', car)
  b <- lp(a, car, smooth = 0.01, NULL)
  x <- multi_bnc_bn(a, car, smooth = 0.01)
  # TODO: what to do about this call?
  b$.call_bn <- x[[1]]$.call_bn <- NULL
  expect_identical(b, x[[1]])
  
  c <- compute_augnb_luccpx(b, car)
  y <- multi_compute_augnb_luccpx(b, car)
  expect_equal(c, y[[1]])
})