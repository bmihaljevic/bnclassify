context("learn params manb")

# The exact posterior probabilities compare to were obtained using the MANB implementation by Wei et al. 

test_that("compute manb nominal", {
  nb <- nbcar()
  u <- lapply(families(nb), extract_ctgt, car)[features(nb)]
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1)
  expect_named(d, features(nb))
  d <- as.vector(d)
  expect_equal(d, c(1, 1, 0.000026294701543, 1, 1, 1))
})

test_that("compute manb no features", {
  nb <- nbcarclass()
  a <- list()
  names(a) <- character()
  expect_equivalent(compute_manb_arc_posteriors(nb, a, smooth = 1), numeric())
})

test_that("compute manb prior", {
  nb <- nbcar()
  u <- lapply(families(nb), extract_ctgt, car)[features(nb)]
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1, prior = 0.00001)
  expect_named(d, features(nb))
  d <- as.vector(d)
  expect_equal(d, c(1, 1, 0.000000000262957, 1, 0.999981436585993, 1))
  
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1, prior = 0.03)
  expect_named(d, features(nb))
  d <- as.vector(d)
  expect_equal(d, c(1, 1, 0.000000813258915, 1, 0.999999993997562, 1))
  
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1, prior = 0.95)
  expect_named(d, features(nb))
  d <- as.vector(d)
  expect_equal(d, c(1, 1, 0.000499362978504, 1, 0.999999999990223, 1))
})

test_that("compute manb smooth", {
  nb <- nbcar()
  u <- lapply(families(nb), extract_ctgt, car)[features(nb)]
  # No error for smooth not being integer. It is close to when smooth = 1
  d <- compute_manb_arc_posteriors(nb, u, smooth = 0.99)
  d1 <- compute_manb_arc_posteriors(nb, u, smooth = 1)
  expect_true(sum(abs(d - d1)) < 1e-5)
  expect_error(compute_manb_arc_posteriors(nb, u, smooth = 0), " > 0 is not TRUE")
})


test_that("compute manb not nb", {
  tn <- tan_cl('class', car)
  ctgts <- lapply(families(tn), extract_ctgt, car)[features(tn)]
  expect_error(compute_manb_arc_posteriors(tn, ctgts, smooth = 1), 
               "MANB can only be applied to naive Bayes")
})

test_that("compute cpt", {
  a <- extract_ctgt(c('doors', 'class'), car)
  b <- compute_manb_cpt(a, 1, smooth = 0)
  a <- ctgt2cpt(a, 0)
  expect_equal(b, a)
  
  a <- extract_ctgt(c('doors', 'class'), car)
  b <- compute_manb_cpt(a, 1, smooth = 1)
  a <- ctgt2cpt(a, 1)
  expect_equal(b, a)
  
  p <- extract_ctgt(c('buying', 'class'), car)
  u <- compute_manb_cpt(p, 0, smooth = 1)
  t <- ctgt2cpt(p, smooth = 1)
  t[] <- ctgt2cpt(extract_ctgt(c('buying'), car), smooth = 1)
  expect_equal(u, t)
  
  p <- extract_ctgt(c('buying', 'class'), car)
  u <- compute_manb_cpt(p, 0.5, smooth = 1)
  t <- ctgt2cpt(p, smooth = 1)
  pt <- t
  pt[] <- ctgt2cpt(extract_ctgt(c('buying'), car), smooth = 1)
  pt[] <- (pt + t) / 2
  expect_equal(u, pt)
})