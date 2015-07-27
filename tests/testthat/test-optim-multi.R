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
  skip_on_cran()
  a <- nb('class', car)
  b <- lp(a, car, smooth = 0.01)
  x <- multi_bnc_bn(a, car, smooth = 0.01)
  # TODO: what to do about this call?
  b$.call_bn <- x[[1]]$.call_bn <- NULL
  expect_identical(b, x[[1]])
  
  c <- compute_anb_log_joint_per_class(b, car)
  y <- multi_compute_log_joint_per_class(b, car)
  expect_equal(c, y[[1]])
})

test_that("multi class posterior nominal", {
  skip_on_cran()
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  cr <- multi_compute_log_joint_per_class(list(a, b), car)  
  ar <- compute_anb_log_joint_per_class(a, car)
  expect_equal(cr[[1]], ar)
  br <- compute_anb_log_joint_per_class(b, car)
  expect_equal(cr[[2]], br)
})

test_that("multi class posterior single bnc", {
  a <- nbcar()  
  b <- multi_compute_log_joint_per_class(a, car)  
  c <- compute_anb_log_joint_per_class(a, car)
  expect_equal(b[[1]], c)
})

test_that("multi class posterior 0 row db", {
  a <- nbcar()  
  b <- multi_compute_log_joint_per_class(a, car[FALSE, ])  
  expect_equal(dim(b[[1]]), c(0L, 4L))
})