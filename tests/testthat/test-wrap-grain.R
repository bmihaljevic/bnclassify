context("grain")

test_that("nominal unnormalized class posterior instance", { 
  skip_if_not_installed('gRain')
  a <- nbvote()
  g <- as_grain(a)
  vc <- as.matrix(voting)
  cp <- compute_grain_uccpx_instance(vc[1, -17], g, 'Class')
  expect_true(is.numeric(cp))
  expect_equal(names(cp), levels(voting$Class))
  expect_equal(as.vector(cp), c(1.289035e-07, 9.999999e-01), tolerance = 1e-7)
})

test_that(" unnormalized class posterior instance with no evidence", { 
  skip_if_not_installed('gRain')
  a <- nbvote()
  g <- as_grain(a)
  vc <- as.matrix(voting)
  inst <- vc[1, -17]
  inst[] <- NA
  cp <- compute_grain_uccpx_instance(inst, g, 'Class')
  expect_true(is.numeric(cp))
  expect_equal(names(cp), levels(voting$Class))
  expect_true(equivalent_num(cp, params(a)[class_var(a)][[1]]))
})

test_that("cpts to grain", {
  skip_if_not_installed('gRain')
  a <- nbvote()
  g <- compile_grain(params(a))
  # Check compiled
  expect_true(g$isCompiled)
  p <- gRain::predict.grain(g, 'Class', newdata = voting[, -17] )
  expect_true(all(p$pred[[1]] %in% levels(voting$Class)))
})