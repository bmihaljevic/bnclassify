context("augnb log unnormalized class posterior")

test_that("Nominal", {
  tn <- nbcar()
  a <- compute_augnb_luccpx(tn, car)
  expect_identical(colnames(a), levels(car$class))
})

test_that("Missing features", {
  tn <- nbcar()
  expect_error(compute_augnb_luccpx(tn, car[, 1:2]), "undefined")
})

test_that("Single predictor", {
  tn <- bnc_bn(nb('class', car[, c(1,7)]), car, smooth = 0)
  pt <- compute_augnb_luccpx(tn, car[, 1:2])
  expect_identical(dim(pt), c(nrow(car), 4L))
})

test_that("0 rows dataset", {
  tn <- nbcar()
  pt <- compute_augnb_luccpx(tn, car[FALSE, ])
  expect_identical(dim(pt), c(0L, 4L))
})

test_that("make cpt inds", {
  tn <- bnc_bn(nb('class', car), car, smooth = 0)
  # Nominal
  tinds <- make_xcpt_indices(features(tn), class_var(tn), 4L, car)
  expect_equal(names(tinds), colnames(car))
  expect_equal(length(tinds), 7)
  expect_equal(length(tinds[[1]]), nrow(car) * 4)
  expect_equal(tinds$class, rep(1:4, each = nrow(car)))
  # 0 vars
  expect_error(make_xcpt_indices(list(), 'class', 4L, car), 'character')
})

test_that("matches grain", {
  skip_on_cran()
  skip_if_not_installed('gRain')
  tn <- nbcar()
  b <- compute_augnb_luccpx(tn, car)
  g <- as_grain(tn)
  gp <- compute_grain_luccpx(grain = g, car[, -7], 'class')
  expect_equal(b, gp) 
  
  tn <- nbvotecomp()
  b <- compute_augnb_luccpx(tn, v)
  g <- as_grain(tn)
  gp <- compute_grain_luccpx(grain = g, v[, -17], 'Class')
})

test_that("fail with incomplete data", {
  v <- nbvote()
  expect_error(compute_augnb_luccpx(v, voting), "anyNA")
})

test_that("multi class posterior nominal", {
  a <- nbcar()  
  b <- nbcarp(car[, 4:7])
  cr <- multi_compute_augnb_luccpx(list(a, b), car)  
  ar <- compute_augnb_luccpx(a, car)
  expect_equal(cr[[1]], ar)
  br <- compute_augnb_luccpx(b, car)
  expect_equal(cr[[2]], br)
})

test_that("multi class posterior single bnc", {
  a <- nbcar()  
  b <- multi_compute_augnb_luccpx(a, car)  
  c <- compute_augnb_luccpx(a, car)
  expect_equal(b[[1]], c)
})

# Not implemented to receive just CPTs
# test_that("compute augnb lucp", {
#   df <- alphadb
#   vars <- list(letters[1:3], c(letters[4:6], letters[3]))
#   rct <- lapply(vars, extract_cpt, df, smooth = 0)
#   rcp <- extract_cpt('c', df, smooth = 0)
#   compute_augnb_lucp(rct, rcp, x = df)
# })
