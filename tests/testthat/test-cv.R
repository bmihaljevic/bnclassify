context("cv")

test_that("CV a structure with no fitting args", {
  n <- nb('class', car)
  expect_error(cv(n, car, k = 5, dag = FALSE), "elements")
})

test_that("CV a bnc_bn", {
  n <- lp(nb('class', car), car, 1)
  a <- cv(n, car, k = 5, dag = FALSE)
  expect_true( a > 0.5)
})

test_that("CV two bnc_bns for fitting", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  a <- cv(list(n, m), car, k = 5, dag = FALSE)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))
})

test_that("CV means = FALSE", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  a <- cv(list(n, m), car, k = 5, dag = FALSE, mean = FALSE)
  expect_equal(ncol(a), 2L)
  expect_equal(nrow(a), 5L)
  expect_true(all(colMeans(a) > 0.5))  
})
          

test_that("CV two bnc_bns to repeat learning", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  # Fitting does not fail on just 2 columns, as dag is re-learned on 
  # those columns
  a <- cv(list(n, m), car[ , c(1, 7)], k = 5, dag = TRUE)
  expect_equal(length(a), 2L)
  expect_true(all(a > 0.5))  
  # With dag = FALSE it fails
  expect_error(cv(list(n, m), car[ , c(1, 7)], k = 5, dag = FALSE), "not found")
})

test_that("CV classifier names", {
  n <- lp(nb('class', car), car, 1)
  m <- lp(nb('class', car[, c(1, 3, 7)]), car, 1)
  a <- cv(list(a = n, b = m), car[ , c(1, 7)], k = 5, dag = TRUE)
  expect_equal(names(a), letters[1:2])
})

test_that("Fast structure fitting with smooth", {
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car[, 7, drop = FALSE]), car, smooth = 1)	 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  r <- cv(list(a, b), car, k = 10, dag = FALSE)
  expect_equal(r, c(0.8582183, 0.7002446), tolerance = 1e-7) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  s <- cv(list(a, b), car, k = 10, dag = FALSE)
  expect_equal(s, r)
})

test_that("CV a wrapper", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  t <- tan_hc('class', car, k = 2, epsilon = 0, smooth = 0.01)
  t <- lp(t, car, smooth = 0.01)
  r <- cv(t, car, k = 2, dag = TRUE)
  expect_equal(r, 0.9346065, tolerance = 1e-7)
}) 

test_that("CV aode", {
  skip_on_cran() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  t <- aode('class', car)
  t <- lp(t, car, smooth = 0.01)
  r <- cv(t, car, k = 2, dag = TRUE)
  expect_equal(r, 0.9056713, tolerance = 1e-7) # just regression test, comparing value I obtained on first run
})

test_that("correct cv result", {
  skip_on_cran()
  t <- tan_hc('class', car, k = 5, epsilon = 0, smooth = 0.12)
  t <- lp(t, car, smooth = 0.01) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  s <- cv(t, car, k = 5, dag = TRUE)
  expect_equal(s, 0.9415345, tolerance = 1e-6)
})

test_that("cv with different parameter learning", {
  a <- lp(nb('class', car), car, smooth = 1)	 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  b <- lp(nb('class', car), car, smooth = 1, awnb_trees = 10)
  d <- lp(nb('class', car), car, smooth = 1, manb_prior = 0.1)	
  r <- cv(list(a, b, d), car, k = 5, dag = FALSE)
  expect_true(r[1] > r[2])
  expect_true(r[3] > r[2])
  expect_true(r[1] > r[3])
})

test_that("correct cv result with missing data", {
  skip_on_cran()
  skip_if_not_installed('gRain')
  nb <- nbvote() 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0) 
  # gRain implementation change
  # s <- cv(nb, voting, k = 5, dag = TRUE)
  # expect_equal(s, 0.9014301, tolerance = 1e-6)   
  # gRain implementation change
  expect_error(s <- cv(nb, voting, k = 5, dag = TRUE))
})

test_that("cv with just-class classifier", {
  skip_on_cran()
  a <- lp(nb('class', car), car, smooth = 1)	
  b <- lp(nb('class', car[, 'class', drop = FALSE]), car, smooth = 1)
  d <- lp(nb('class', car[, c(sample(1:6, 4), 7), drop=FALSE]), car, smooth=1) 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  r <- cv(list(a, b, d), car, k = 10, dag = TRUE)
})

test_that("learn and asses nominal", {
  n <- nbcar()
  mem_cpt <- make_cpts_cache(car, smooth = 1)
  a <- learn_and_assess(mem_cpt, car, n)
  p <- accuracy(predict(n, car), car$class)
  expect_equal(a, p)
})

test_that("cv fixed partition", {
  n <- nbcar()
  mem_cpt <- make_cpts_cache(car, smooth = 1)
  a <- cv_lp_partition(n, list(mem_cpt, mem_cpt), list(car, car))
  p <- accuracy(predict(n, car), car$class)
  expect_equal(a, p)
  
  d <- tan_cl('class', car)
  a <- cv_lp_partition(list(n, d), list(mem_cpt, mem_cpt), list(car, car))
  p <- accuracy(predict(n, car), car$class)
  g <- accuracy(predict(lp(d, car, smooth = 1), car), car$class)
  expect_equal(a, c(p, g))
})

test_that("make folds nominal", {
  test_make_stratified <- function() {
    f <- make_stratified_folds(car$class, 3)
    tf <- sapply(f, length)
    expect_true(max(tf) < 578, min(tf) > 574)
    tbl <- function(a) {
      table(car$class[a])
    }
    a <- lapply(f, tbl)
    diffs <- sum(abs(a[[1]] - a[[2]]), abs(a[[1]] - a[[3]]))
    expect_true(diffs <= 5)
  }
  replicate(10, test_make_stratified)
})

test_that("make folds empty class", {
  car_cv <- car[1:300, ]
  f <- make_stratified_folds(car_cv$class, 2)
  a <- table(car$class[f[[1]]])
  b <- table(car$class[f[[2]]])
  expect_equal(sum(abs(a - b)), 0)
})

test_that("distribute accross folds nominal", { 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(4)
  f <- distribute_class_over_folds(1210, 3)
  expect_true(all(table(f) == c(403, 404, 403)))
  expect_error(f <- distribute_class_over_folds(1, 1))
  f <- distribute_class_over_folds(4, 4)
  expect_equal(sort(f), 1:4)
  f <- distribute_class_over_folds(1, 2)
  expect_equal(f, 1)
  f <- distribute_class_over_folds(4, 1200)
  expect_equal(f, c(733, 485, 899, 857))
  f <- distribute_class_over_folds(0, 2)
  expect_equal(f, integer())
})

test_that("cv of different models", {  
  skip_on_cran()
  t <- kdb('class', dataset = car, kdb = 1, k = 10, epsilon = 0)
  t <- lp(t, car, smooth = 1) 
  to <- tan_hc('class', dataset = car, k = 10, epsilon = 0)
  to <- lp(to, car, smooth = 1)  
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0) 
  score <- cv(list(t, to), car, k = 2)
  expect_false(isTRUE(all.equal(score[1], score[2])))
})