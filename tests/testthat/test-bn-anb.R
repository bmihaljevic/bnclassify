context("bn-anb")

data(car)

test_that(" not a error with loglik", {
  bnanb<-bn_anb('class', car, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
})

test_that("error with bic",{
  expect_error( bn_anb('class', car, score = 'bic'), "length(dm) >= 3L is not TRUE" )
})

test_that("error with aic",{
  expect_error( bn_anb('class', car, score = 'bic'), "length(dm) >= 3L is not TRUE" )
})