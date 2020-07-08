context("bn-anb")

data(car)

test_that(" not a error with loglik", {
  bnanb<-bn_anb('class', car, score = 'loglik' )
  expect_equal(class(bnanb), c("bnc_dag", "bnc_base"))
})


test_that("error with aic",{
  expect_error( bn_anb('Species', iris, score = 'bic'), "length(dm) >= 3L is not TRUE" )
})  

test_that("negative mutual information",{ 
  mutual_information("Species", "Sepal.Length", iris)
  mutual_information(c("Species", "Sepal.Width"), "Sepal.Length", iris)
}) 

test_that("non-symmetric mutual information",{ 
  mutual_information("Species", "Sepal.Length", iris)
  mutual_information("Sepal.Length", "Species",  iris)
})

test_that("family score becomes negative after adding a parent",{ 
  family_scores("Species", "Sepal.Length", iris, score = 'loglik')
  family_scores(c("Species", "Sepal.Width"), "Sepal.Length", iris, score = 'loglik')
}) 