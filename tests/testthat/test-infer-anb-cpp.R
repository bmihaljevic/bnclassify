context("infer-anb-cpp") 

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

