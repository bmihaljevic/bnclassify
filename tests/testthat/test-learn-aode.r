context("aode")

test_that("spode", {
  u <- spode(sp='D', features=LETTERS[5:9], class = 'C')
  expect_equal(graph::numEdges(as_graphNEL(u)), 11)
  expect_equal(length(graph::adj(as_graphNEL(u), 'D')$D), 5)
  
  u <- spode(sp='E', features=LETTERS[c(4, 6:9)], class = 'C')
  expect_equal(graph::numEdges(as_graphNEL(u)), 11)
  expect_equal(length(graph::adj(as_graphNEL(u), 'E')$E), 5)
})

test_that("aode str", {  
  # with 1 feature is an nb
  u <- aode(class = 'a', alphadb[, 1:2, drop = FALSE])  
  expect_true(is_ode(u))  
  
  u <- aode(class = 'a', random_letters_db(10))  
  expect_true(is_aode(u))    
  expect_true(is_ode(u$models[['c']]))  
  expect_equal(length(u$models), 9)
  # expect_identical(class_var(u), 'A')
  # TODO 
  # expect_identical(features(u), LETTERS[2:10])
  d <- u$models[[1]]
  expect_equal(graph::numEdges(as_graphNEL(d)), 9 + 8)
  expect_equal(length(graph::adj(as_graphNEL(d), 'b')$b), 8)
  d <- u$models[[9]]
  expect_equal(graph::numEdges(as_graphNEL(d)), 9 + 8)
  expect_equal(length(graph::adj(as_graphNEL(d), 'j')$j), 8)
})

# test_that("fit aode and bnc", {
#   a <- bnc(car, learner="aode")
#   sapply(a$models, BIC, car) # No error
# })  

test_that("predict", {  
  a <- aode('class', car, m=10000)  
  a <- lp(a, car, smooth = 1) 
  
  a <- bnc('aode', 'class', car, dag_args = list(m=10000), smooth = 1)  
  p <- predict(a, car, prob = TRUE)  
  expect_equal(sum(abs(apply(p, 1, '-', class_prior(a)))), 0)
  
  a <- bnc(car, learner=list("aode"), smooth=1)  
  nb <- bnc(car)  
  p <- predict(a, car, prob = TRUE)  
  expect_equal(p[12, 1], c(unacc=0.793), tolerance = 0.001) # Probability from Weka     
  
  a <- bnc(dbreast, learner=list("aode"), smooth=1)  
  p <- predict(a, dbreast, prob = TRUE)  
  expect_equal(p[1, 1], c("no-recurrence-events"=0.494), tolerance = 0.01) # Weka has 0.494   
})
