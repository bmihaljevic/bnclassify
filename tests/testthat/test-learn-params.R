context("Learn params")

test_that("learn params", {
  # Check call and environment
  n <- nb('class', car)
  nf <- lp(n, car, smooth = 1)
  nf2 <- bnc_update(bnc_get_update_args(nf, dag = FALSE), car)
  expect_identical(nf, nf2)
})

test_that("Uniform probability instead of 0", {
  cbs <- car[1:4, c('buying', 'safety')]
  nb <- lp(nb('buying', cbs), cbs, smooth = 0)
  p <- as.vector(params(nb)[['safety']][, 'low'])
  expect_equal(p, rep(0.33333333, 3))
})

test_that("Smoothing", {  
  cbs <- car[1:4, c('buying', 'safety')]
  nb <- lp(nb('safety', cbs), cbs, smooth = 0)
  expect_equivalent(params(nb)[['safety']]['low'], 0.5)
  nb <- lp(nb('safety', car[, c('buying', 'safety')]), car[1:4, ], smooth = 1)
  expect_equivalent(params(nb)[['safety']]['low'], 3/7)
})

test_that('Set feature weights', {
  nb <- nbcar()
  w <- structure(rep(0.5, 6), names = features(nb))
  f <- set_weights(nb, w)
  expect_equal(params(f)$class, params(nb)$class)
  expect_true(all(params(f)$buying != params(nb)$buying))
  expect_true(are_pdists(t(params(f)$buying)))
})

test_that('awnb nominal', {
  nb <- nbcar()
  a <- lp(nb, car, smooth = 1, awnb_trees = 1, awnb_bootstrap = 1) 
  b <- lp(nb, car, smooth = 1)
  expect_equal(params(a)$class, params(b)$class)
  expect_true(all(params(a)$buying != params(b)$buying))
  expect_true(are_pdists(t(params(a)$buying)))
  expect_equal(a$.call_bn[[1]], "lp")
})

test_that("awnb do not call", {
  nb <- nbcar()
  a <- lp(nb, car, smooth = 1, awnb_trees = NULL, awnb_bootstrap = NULL) 
  b <- lp(nb, car, smooth = 1)
  identical_non_call(a, b)
})

test_that('awnb default params', {
  nb <- nbcar()
  set.seed(0)
  a <- lp(nb, car, smooth = 1, awnb_trees = 10, awnb_bootstrap = 0.5) 
  set.seed(0)
  b <- lp(nb, car, smooth = 1, awnb_trees = 10) 
  identical_non_call(a, b)
  set.seed(0)
  b <- lp(nb, car, smooth = 1, awnb_bootstrap = 0.5) 
  identical_non_call(a, b)
})

test_that("awnb Incomplete data" , {
  a <- nb('Class', voting)
  b <- lp(a, voting, smooth = 1, awnb_trees = 1, awnb_bootstrap = 0.1)
  c <- lp(a, voting, smooth = 1)
  expect_equal(params(b), params(set_weights(c, b$.weights)))
})

test_that('bnc function nominal', {
  a <- bnc('nb', 'class', car, smooth = 1)
  b <- lp(nb('class', car), car, smooth = 1)
  expect_identical(a, b)
})

test_that('bnc with args', {
  a <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(root = 'safety'))
  b <- lp(tan_cl('class', car, root = 'safety'), car, smooth = 1)
  expect_identical(a, b)
})

test_that('lp_implement with cache nominal', {
  n <- nb('class', car)
  a <- make_cpts_cache(car, smooth = 0.04)
  e <- lp_implement(n, .mem_cpts = a)
  b <- lp_implement(n, car, smooth = 0.04)
  expect_identical(e, b)
})