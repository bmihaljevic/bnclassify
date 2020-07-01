context("bn-anb")

test_that(" not a error with loglik", {
  bnanb<-bn_anb('class', iris, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
})

test_that("error with bic",{
  expect_error( bn_anb('class', iris, score = 'bic'), "Error in cmi_degrees_freedom(freqs) : length(dm) >= 3L is not TRUE" )
})

test_that("error with aic",{
  expect_error( bn_anb('class', iris, score = 'bic'), "Error in cmi_degrees_freedom(freqs) : length(dm) >= 3L is not TRUE" )
})