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
  suppressWarnings(RNGversion("3.5.0"))
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
  expect_equal(params(b), params(set_weights(c, awnb_weights(b))))
})

test_that('bnc function nominal', {
  a <- bnc('nb', 'class', car, smooth = 1)
  b <- lp(nb('class', car), car, smooth = 1, awnb_trees = NULL, 
          awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL)
  expect_identical(a, b)
})

test_that('bnc with args', {
  a <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(root = 'safety'))
  b <- lp(tan_cl('class', car, root = 'safety'), car, smooth = 1, 
          awnb_trees = NULL, awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL)
  expect_identical(a, b)
})

test_that('bnc with args and awnb', {
  set.seed(0)
  a <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(root = 'safety'),
           awnb_trees = 10)
  set.seed(0)
  b <- lp(tan_cl('class', car, root = 'safety'), car, smooth = 1, 
          awnb_trees = 10, awnb_bootstrap = NULL, manb_prior = NULL, wanbia = NULL)
  expect_identical(a, b)
})

test_that('lp_implement with cache nominal', {
  n <- nb('class', car)
  a <- make_cpts_cache(car, smooth = 0.04)
  e <- lp_implement(n, .mem_cpts = a)
  b <- lp_implement(n, car, smooth = 0.04)
  expect_identical(e, b)
})

test_that('either awnb or manb', {
  n <- nb('class', car)
  expect_error(lp(n, car, smooth = 1, awnb_trees = 2, manb_prior = 0.3),
               "Either MANB, AWNB, WANBIA can be applied, not more than one.")
  expect_error(lp(n, car, smooth = 1, awnb_bootstrap = 1, manb_prior = 0.3),
               "Either MANB, AWNB, WANBIA can be applied, not more than one.")
})

test_that("manb nominal", {
  nb <- nbcar()
  manb <- lp(nb, car, smooth = 1, manb_prior = 0.5)
  expect_equivalent(manb$.manb, c(1, 1, 0.000026294701543, 1, 1, 1)) 
  expect_equal(names(manb$.params), names(nb$.params))
  expect_equal(sum(abs(manb$.params[['doors']] - nb$.params[['doors']])), 
               0.3950593, tolerance = 1e-7)
  
  nb <- nbcar()
  manb <- lp(nb, car, smooth = 1)
  expect_null(manb$.manb)
  expect_identical(manb$.params, nb$.params)
})

test_that("check manb predictions match wei java implementation", {
  nb <- lp(nb('class', car), car, smooth = 1)
  manb <- lp(nb, car, smooth = 1, manb_prior = 0.5)
  p <- predict(manb, car, prob = TRUE)
  expect_equal(as.vector(p[12, 2]), 0.301507, tolerance = 0.0000002)
  expect_equal(as.vector(p[1646, 2]), 0.307484, tolerance = 0.000002)
  expect_equal(as.vector(p[1728, 2]), 0.209418, tolerance = 0.000002)
  
  nb <- lp(nb('class', car), car, smooth = 1)
  manb <- lp(nb, car, smooth = 1, manb_prior = 0.00001)
  p <- predict(manb, car, prob = TRUE)
  expect_equal(as.vector(p[12, 2]), 0.301510, tolerance = 0.000002)
  expect_equal(as.vector(p[18, 2]), 0.418681, tolerance = 0.000002)
})

test_that("wanbia", {  
  n <- nb('Class', v)
  w <- lp(n, v, smooth = 1, wanbia = TRUE)
  nb <- lp(n, v, smooth = 1)
  expect_lt(sum(abs(params(w)$anti_satellite_test_ban - 0.5)), 1e-10) 
  expect_lt(compute_cll(nb, v), compute_cll(w, v))   
  
  # For car no weights seem to improve 
  n <- nb('class', car)
  nb <- lp(n, car, smooth = 1)
  w <- lp(n, car, smooth = 1, wanbia = TRUE)
  expect_equal(compute_cll(nb, car), compute_cll(w, car))  
}) 
