context("HC bsej")

test_that("Merge supernodes", {
  #  A NB with 6 features yields 15 models 
  nb <- nbcar()
  states <- merge_supernodes(nb)
  expect_equal(length(states), 15)    
  m <- states[[1]]
  expect_equal(narcs(m), length(features(nb)) + 1)  
  # A single predictor
  nb <- nbcarp(car[, 6:7])
  states <- merge_supernodes(nb)
  expect_true(is.null(states))  
  # No predictors   
  nb <- nbcarclass()
  states <- merge_supernodes(nb)
  expect_true(is.null(states))
  
  # relating non-singleton feature subsets
  nb <- nbcar()
  states <- merge_supernodes(nb)  
  e <- merge_supernodes(states[[1]])  
  expect_equal(length(e), 10)
  expect_equal(families(e[[1]])$buying, c("buying", "maint",  "doors",  "class"))
  f <- merge_supernodes(e[[10]])  
  expect_equal(length(f), 6)
})

test_that("Excludes", {
  # One dag per feature
  nb <- nbcar()
  cands <- excludes(nb)
  expect_equal(length(cands), length(features(nb)))    
  # single state for a single feature
  nb <- nbcarp(car[, c(1,7)])
  cands <- excludes(nb)
  expect_equal(length(cands), 1)          
  expect_equal(features(cands[[1]]), character())
  
  # NULL for no features 
  nb <- nbcarclass()
  states <- excludes(nb)
  expect_equal(length(states), 0)          
})

test_that("bsej step", {
  nb <- nbcar()
  e <- bsej_step(nb)  
  expect_equal(length(e), 6 + 15)
})