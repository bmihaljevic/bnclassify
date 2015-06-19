context("mlr")

test_that("as mlr", {
  # Creates a learner just for fitting
  nf <- nbcar()
  ml <- as_mlr(nf, dag = FALSE)
  expect_identical(names(ml$par.vals$args), c('lp_fargs'))
  # Creates a learner for structure learning and fitting
  nf <- nbcar()
  ml <- as_mlr(nf, dag = TRUE)
  expect_identical(names(ml$par.vals$args), c('lp_fargs', 'dag_fargs'))
})

test_that("train", {
  library(mlr)
  # mlr needs to be loaded for train() to work; otherwise it will fail because
  # it won't find learner options c("show.learner.output", "on.learner.error",
  # "on.learner.warning"). To have it working without mlr loaded maybe I must
  # specify these in as_mlr()
  t <- mlr::makeClassifTask(id = "compare", data = car, target = 'class', 
                            fixup.data = "no", check.data = FALSE)  
  nf <- nbcar()
  # Train just with fitting
  ml <- as_mlr(nf, dag = FALSE)
  mod = mlr::train(ml, t, subset = sample(nrow(car), 100))
  # Train with structure learning and fitting
  ml <- as_mlr(nf, dag = TRUE)
  mod = mlr::train(ml, t, subset = sample(nrow(car), 100))
  
  detach('package:mlr')
})

test_that("resample", {
  library(mlr)
  
  ctrl = makeFeatSelControlSequential(alpha = 0, method = "sfs")
  rdesc = makeResampleDesc(method = "Holdout")
  ct <- mlr::makeClassifTask(id = "compare", data = car, target = 'class', 
                            fixup.data = "no", check.data = FALSE)  
  nf <- nbcar()
  bnl <- as_mlr(nf, dag = TRUE)
  sfeats = selectFeatures(learner = bnl, task = ct, resampling = rdesc,
                          control = ctrl, show.info = FALSE)
  sfeats$x
  detach('package:mlr')
})