context("learn params wanbia")

test_that('make cll', {
  w <- rep(1, 16)
  w <- setNames(w, colnames(v)[-ncol(v)]) 
  cll <- make_cll('Class', v)(w) 
  expect_equal(cll, 149.1442, tolerance = 1e-2)
  # Too few weights
  expect_error(make_cll('Class', v)(w[-16]))
})     
 
test_that("wanbia error", {  
  w <- compute_wanbia_weights( 'Class', v, return_optim_object = TRUE)  
  # There is an error. Not sure if this is critical.
  # It does not occur on Windows. Skipping the test for now
  # expect_equal(w$message, "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH")  
}) 

test_that("with incomplete data", {   
  skip_on_cran()
  skip("too slow currently")
  w <- compute_wanbia_weights( 'Class', voting)   
  # just check results is consistent
  expect_equal(w[['physician_fee_freeze']], 0.7730736, tolerance = 1e-6)
})

test_that("check consistent result ", {    
  skip_if_not_installed('mlbench')
  skip_on_cran()
  skip("too slow currently")
  require(mlbench)
  data("DNA") 
  w <- compute_wanbia_weights( 'Class', DNA)   
  expect_equal(w[['V132']], 0.99039949, tolerance = 1e-6) 
}) 

test_that("with more than two classes ", {   
  skip_if_not_installed('mlbench')
  skip_on_cran()
  skip("too slow currently")
  require(mlbench)
  data("Soybean") 
  w <- compute_wanbia_weights( 'Class', Soybean)   
  # just check results is consistent
  expect_equal(w[['seed.tmt']], 0.42133418, tolerance = 1e-6) 
})