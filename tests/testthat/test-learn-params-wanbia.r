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
  expect_equal(w$message, "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH") 
  
}) 