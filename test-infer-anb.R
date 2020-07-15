context("infer anb")

test_that("Nominal", {
  tn <- nbcar()
  a <- compute_joint(tn, car)
  expect_identical(colnames(a), levels(car$class))
})

test_that("Missing features", {
  tn <- nbcar()
  expect_error(compute_joint(tn, car[, 1:2]), 
               "Some features missing from data set.")
})

test_that("Single predictor", {
  tn <- lp(nb('class', car[, c(1,7)]), car, smooth = 0)
  pt <- compute_joint(tn, car[, 1:2])
  expect_identical(dim(pt), c(nrow(car), 4L))
})

test_that("0 rows dataset", {
  tn <- nbcar()
  pt <- compute_joint(tn, car[FALSE, ])
  expect_identical(dim(pt), c(0L, 4L))
})

test_that("No features", {
  nb <- bnc_dag(nb_dag('class', NULL), 'class')
  nb <- lp(nb, car, smooth = 1)
  pt <- compute_joint(nb, car)
  expect_equal(as.vector(pt[1, ]), as.vector(log(params(nb)[['class']])))
  
  pt2 <- compute_joint(nb, car[, FALSE])
  expect_equal(pt, pt2)
})  
 
test_that("matches grain", {
  
  # gRain implementation change
  # skip_on_cran()
  # skip_if_not_installed('gRain')
  # 
  # tn <- nbcar()
  # b <- compute_joint(tn, car)
  # g <- as_grain(tn)
  # gp <- compute_grain_log_joint(grain = g, car[, -7], 'class')
  # expect_equal(b, gp) 
  # 
  # tn <- nbvotecomp()
  # b <- compute_joint(tn, v)
  # g <- as_grain(tn)
  # gp <- compute_grain_log_joint(grain = g, v[, -17], 'Class')
  # expect_equal(b, gp)
  # 
  # tn <- bnc('tan_cl', class = 'class', smooth = 1, dataset = car)
  # b <- compute_joint(tn, car)
  # g <- as_grain(tn)
  # gp <- compute_grain_log_joint(grain = g, car[, -7], 'class')
  # expect_equal(b, gp) 
  # gRain implementation change
})

test_that("correct result", {
  carb <- car[, c(1,7)]
  tn <- nbcarp(carb)
  true_log_prob <- log(params(tn)$buying['vhigh', ]) + log(params(tn)$class)
  b <- compute_joint(tn, carb[1, , drop = FALSE])
  expect_equal(as.vector(true_log_prob), as.vector(b[1, ]))
})

test_that("different levels", {
  nb <- nbcar()
  ce <- car
  levels(ce$buying) <-  rev(levels(ce$buying))
  expect_error(compute_log_joint(nb, ce), 
               "Levels in data set must match those in the CPTs ")
})

test_that("fail with incomplete data", {
  v <- nbvote()
  expect_error(compute_joint(v, voting), "NA entries in data set.")
})


# Not implemented to receive just CPTs
# test_that("compute augnb lucp", {
#   df <- alphadb
#   vars <- list(letters[1:3], c(letters[4:6], letters[3]))
#   rct <- lapply(vars, extract_cpt, df, smooth = 0)
#   rcp <- extract_cpt('c', df, smooth = 0)
#   compute_augnb_lucp(rct, rcp, x = df)
# })