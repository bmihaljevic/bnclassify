context("grain")

test_that("nominal joint prob instance", { 
  # skip_if_not_installed('gRain')
  # a <- nbvote()
  # g <- as_grain(a)
  # vc <- as.matrix(voting)
  # cp <- compute_grain_log_joint_instance(vc[1, -17], g, 'Class')
  # expect_true(is.numeric(cp))
  # expect_equal(names(cp), levels(voting$Class))
  # expect_equal(exp(as.vector(cp)), c(1.956045e-09, 1.517449e-02), tolerance = 1e-6)
})

test_that(" joint prob instance with no evidence", { 
  # skip_if_not_installed('gRain')
  # a <- nbvote()
  # g <- as_grain(a)
  # vc <- as.matrix(voting)
  # inst <- vc[1, -17]
  # inst[] <- NA
  # cp <- compute_grain_log_joint_instance(inst, g, 'Class')
  # expect_true(is.numeric(cp))
  # expect_equal(names(cp), levels(voting$Class))
  # expect_true(equivalent_num(exp(cp), params(a)[class_var(a)][[1]]))
})

test_that("cpts to grain", {
  # skip_if_not_installed('gRain')
  # a <- nbvote()
  # g <- compile_grain(params(a))
  # # Check compiled
  # expect_true(g$isCompiled)
  # # gRain cannot handle a row that has all missing features. remove 249.
  # p <- gRain::predict.grain(g, response = 'Class',  
  #                           newdata = voting[-249, -17], type = "class")
  # expect_true(all(p$pred[[1]] %in% levels(voting$Class[-249])))
})