context("Learn params")

test_that("learn params", {
  # Check call and environment
  n <- nb('class', car)
  nf <- lp(n, car, smooth = 1)
  nf2 <- bnc_update(bnc_get_update_args(nf, dag = FALSE), car)
  expect_identical(nf, nf2)
})

test_that("S probability instead of 0", {
  cbs <- car[1:4, c('buying', 'safety')]
  nb <- lp(nb('buying', cbs), cbs, smooth = 0)
  p <- as.vector(bnc_params(nb)[['safety']][, 'low'])
  expect_equal(p, rep(0.33333333, 3))
})

test_that("Smoothing", {  
  cbs <- car[1:4, c('buying', 'safety')]
  nb <- lp(nb('safety', cbs), cbs, smooth = 0)
  expect_equivalent(bnc_params(nb)[['safety']]['low'], 0.5)
  nb <- lp(nb('safety', car[, c('buying', 'safety')]), car[1:4, ], smooth = 1)
  expect_equivalent(bnc_params(nb)[['safety']]['low'], 3/7)
})