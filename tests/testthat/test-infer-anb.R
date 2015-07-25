context("infer anb")

test_that("Nominal", {
  tn <- nbcar()
  a <- compute_anb_log_joint_per_class(tn, car)
  expect_identical(colnames(a), levels(car$class))
})

test_that("Missing features", {
  tn <- nbcar()
  expect_error(compute_anb_log_joint_per_class(tn, car[, 1:2]), 
               "undefined columns selected")
})

test_that("Single predictor", {
  tn <- bnc_bn(nb('class', car[, c(1,7)]), car, smooth = 0)
  pt <- compute_anb_log_joint_per_class(tn, car[, 1:2])
  expect_identical(dim(pt), c(nrow(car), 4L))
})

test_that("0 rows dataset", {
  tn <- nbcar()
  pt <- compute_anb_log_joint_per_class(tn, car[FALSE, ])
  expect_identical(dim(pt), c(0L, 4L))
})

test_that("No features", {
  nb <- bnc_dag(nb_dag('class', NULL), 'class')
  nb <- bnc_bn(nb, car, smooth = 1)
  pt <- compute_anb_log_joint_per_class(nb, car)
  expect_equal(as.vector(pt[1, ]), as.vector(log(params(nb)[['class']])))
  
  pt2 <- compute_anb_log_joint_per_class(nb, car[, FALSE])
  expect_equal(pt, pt2)
})

test_that("make cpt inds nominal", {
  tn <- bnc_bn(nb('class', car), car, smooth = 0)
  # Nominal
  xc <- params(tn)[features(tn)]
  tinds <- x2cpt_inds(xc, car, class_var(tn), 4L)
  expect_equal(names(tinds), colnames(car))
  expect_equal(length(tinds), 7)
  expect_equal(length(tinds[[1]]), nrow(car) * 4)
  expect_equal(tinds$class, rep(1:4, each = nrow(car)))
})

test_that("make cpt inds 0 features", {
  # 0 vars
  tn <- bnc_bn(nb('class', car), car, smooth = 0)
  # Nominal
  xc <- params(tn)[features(tn)]
  expect_equal(x2cpt_inds(list(), car, 'class', 4L), list())
})

test_that("matches grain", {
  skip_on_cran()
  skip_if_not_installed('gRain')
  
  tn <- nbcar()
  b <- compute_anb_log_joint_per_class(tn, car)
  g <- as_grain(tn)
  gp <- compute_grain_log_joint(grain = g, car[, -7], 'class')
  expect_equal(b, gp) 
  
  tn <- nbvotecomp()
  b <- compute_anb_log_joint_per_class(tn, v)
  g <- as_grain(tn)
  gp <- compute_grain_log_joint(grain = g, v[, -17], 'Class')
  expect_equal(b, gp)
  
  tn <- bnc('tan_cl', class = 'class', smooth = 1, dataset = car)
  b <- compute_anb_log_joint_per_class(tn, car)
  g <- as_grain(tn)
  gp <- compute_grain_log_joint(grain = g, car[, -7], 'class')
  expect_equal(b, gp)
})

test_that("correct result", {
  carb <- car[, c(1,7)]
  tn <- nbcarp(carb)
  true_log_prob <- log(params(tn)$buying['vhigh', ]) + log(params(tn)$class)
  b <- compute_anb_log_joint_per_class(tn, carb[1, , drop = FALSE])
  expect_equal(as.vector(true_log_prob), as.vector(b[1, ]))
})

test_that("different levels", {
  nb <- nbcar()
  ce <- car
  levels(ce$buying) <-  rev(levels(ce$buying))
  expect_error(compute_anb_log_joint_per_class(nb, ce), 
               "Levels in data set must match those in the CPTs ")
})

test_that("fail with incomplete data", {
  v <- nbvote()
  expect_error(compute_anb_log_joint_per_class(v, voting), "anyNA")
})

test_that("cpt var values nominal", {
  a <- nbcar()
  cpts <- params(a)
  vv <- cpt_vars_values2(cpts)
  expect_equal(names(vv), names(vars(a)))
  expect_equal(vv$safety, c("high", "low", "med"))
})


# Not implemented to receive just CPTs
# test_that("compute augnb lucp", {
#   df <- alphadb
#   vars <- list(letters[1:3], c(letters[4:6], letters[3]))
#   rct <- lapply(vars, extract_cpt, df, smooth = 0)
#   rcp <- extract_cpt('c', df, smooth = 0)
#   compute_augnb_lucp(rct, rcp, x = df)
# })