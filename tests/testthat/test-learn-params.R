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