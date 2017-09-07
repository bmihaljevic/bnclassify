test_that("spode", {
  u <- spode(sp='D', features=LETTERS[5:9], class = 'C')
  expect_equal(graph::numEdges(u), 11)
  expect_equal(length(graph::adj(u, 'D')$D), 5)
  
  u <- spode(sp='E', features=LETTERS[c(4, 6:9)], class = 'C')
  expect_equal(graph::numEdges(u), 11)
  expect_equal(length(graph::adj(u, 'E')$E), 5)
})

test_that("aode str", {  
  # with 1 feature is an nb
  u <- aode(class = 'A', LETTERS[2])  
  expect_true(is_ode(u))  
  
  u <- aode(class = 'A', LETTERS[2:10])  
  expect_true(is_aode(u))    
  expect_true(is_ode(u$models[['C']]))  
  expect_equal(length(u$models), 9)
  expect_identical(class_var(u), 'A')
  expect_identical(features(u), LETTERS[2:10])
  d <- u$models[[1]]
  expect_equal(graph::numEdges(bnc_dag(d)), 9 + 8)
  expect_equal(length(graph::adj(bnc_dag(d), 'B')$B), 8)
  d <- u$models[[9]]
  expect_equal(graph::numEdges(bnc_dag(d)), 9 + 8)
  expect_equal(length(graph::adj(bnc_dag(d), 'J')$J), 8)
})

# test_that("fit aode and bnc", {
#   a <- bnc(car, learner="aode")
#   sapply(a$models, BIC, car) # No error 
# })
