context("learn params manb")

test_that("compute manb nominal", {
  nb <- nbcar()
  a <- extract_ctgt(c('doors', 'class'), car)
  vars <- lapply(features(nb), c, 'class')
  u <- lapply(vars, extract_ctgt, car)
  names(u) <- features(nb)
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1)
  expect_equivalent(d, c(1, 1, 0.000026294701543, 1, 1, 1))
})

test_that("compute manb smooth", {
  nb <- nbcar()
  nb <- nbcar()
  a <- extract_ctgt(c('doors', 'class'), car)
  vars <- lapply(features(nb), c, 'class')
  u <- lapply(vars, extract_ctgt, car)
  names(u) <- features(nb)
  # No error for smooth not being integer. It is close to when smooth = 1
  d <- compute_manb_arc_posteriors(nb, u, smooth = 0.99)
  d1 <- compute_manb_arc_posteriors(nb, u, smooth = 1)
  expect_true(sum(abs(d - d1)) < 1e-5)
  expect_error(compute_manb_arc_posteriors(nb, u, smooth = 0), " > 0 is not TRUE")
})