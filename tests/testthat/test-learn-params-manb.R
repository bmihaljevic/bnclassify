context("learn params manb")

# The exact posterior probabilities compare to were obtained using the MANB implementation by Wei et al. 

test_that("compute manb nominal", {
  nb <- nbcar()
  a <- extract_ctgt(c('doors', 'class'), car)
  vars <- lapply(features(nb), c, 'class')
  u <- lapply(vars, extract_ctgt, car)
  names(u) <- features(nb)
  d <- compute_manb_arc_posteriors(nb, u, smooth = 1)
  expect_named(d, features(nb))
  d <- as.vector(d)
  expect_equal(d, c(1, 1, 0.000026294701543, 1, 1, 1))
})

test_that("compute manb no features", {
  nb <- nbcarclass()
  expect_equal(compute_manb_arc_posteriors(nb, list(), smooth = 1), numeric())
})

test_that("compute manb prior", {
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


test_that("compute manb not nb", {
  tn <- tan_cl('class', car)
  ctgts <- families2ctgts(families(tn), car)[features(tn)]
  expect_error(compute_manb(tn, ctgts, smooth = 1), 
               "MANB can only be applied to naive Bayes")
})