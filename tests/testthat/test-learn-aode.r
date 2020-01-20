context("aode")

test_that("spode", {
  u <- spode(sp='D', features=LETTERS[5:9], class = 'C')
  expect_equal(graph_num_arcs(dag(u)), 11)
  expect_equal(length(graph_get_adjacent('D', dag(u))), 6)
  
  u <- spode(sp='E', features=LETTERS[c(4, 6:9)], class = 'C')
  expect_equal(graph_num_arcs(dag(u)), 11)
  expect_equal(length(graph_get_adjacent('E', dag(u))), 6)
})

test_that("aode str", {  
  # with 1 feature is an nb
  u <- aode(class = 'a', alphadb[, 1:2, drop = FALSE])  
  expect_true(is_nb(u))  
  
  u <- aode(class = 'a', random_letters_db(10))  
  expect_true(is_aode(u))    
  expect_true(is_ode(models(u)[['c']]))  
  expect_equal(length(models(u)), 9)
  expect_identical(class_var(u), 'a') 
  expect_identical(features(u), letters[2:10])
  
  d <- models(u)[[1]]
  expect_equal(graph_num_arcs(dag(d)), 9 + 8)   
  expect_equal(length(graph_get_adjacent('b', dag(d))), 9)
  d <- models(u)[[9]]
  expect_equal(graph_num_arcs(dag(d)), 9 + 8)
  expect_equal(length(graph_get_adjacent('j', dag(d))), 9)
}) 

test_that("aode bnc funs", {    
  lets <- random_letters_db(10)
  u <- aode(class = 'a', lets)  
  expect_equal(class_var(u), 'a')
  feats <- setdiff(colnames(lets), 'a') 
  expect_equal(features(u), feats)
  expect_false(is_ode(u)) 
  expect_false(is_semi_naive(u)) 
  expect_false(is_nb(u))  
  expect_error(narcs(u))
  expect_error(nparams(u))  
  expect_error(params(u)) 
  expect_equal(length(models(u)), 9)
  expect_output(print(u), regexp = 'ensemble of 9') 
  expect_output(print(u), regexp = 'learning algorithm:    aode') 
})

test_that("fit aode and bnc", {
  a <- bnc('aode', 'class', car, smooth = 1)
  sapply(a$models, BIC, car) # No error
})

test_that("log joint", {   
  a <- aode('class', car)  
  a <- lp(a, car, smooth = 1)
  lj <- compute_log_joint(a, car)
  expect_equal(sum(exp(lj)), 1)     
})

test_that("predict", {  
  # currently not considering weights
  # a <- aode('class', car, m=10000)  
  # a <- lp(a, car, smooth = 1) 
 
  # a <- bnc('aode', 'class', car, dag_args = list(m=10000), smooth = 1)  
  # p <- predict(a, car, prob = TRUE)  
  # cp <- params(a$models[[1]])$class
  # expect_equal(sum(abs(apply(p, 1, '-', cp))), 0)
  
  a <- bnc('aode', 'class', car, smooth=1)  
  p <- predict(a, car, prob = TRUE)  
  expect_equal(p[12, 1], c(unacc=0.793), tolerance = 0.001) # Probability from Weka     
 
  # dbreast <- foreign::read.arff('~/code/teach-asdm-c01/data/dbreast-cancer.arff')
  # a <- bnc('aode', 'Class', dbreast, smooth=1)  
  # p <- predict(a, dbreast, prob = TRUE)  
  # expect_equal(p[1, 1], c("no-recurrence-events"=0.494), tolerance = 0.01) # Weka has 0.494   
})  

test_that("incomplete data", {    
  # gRain implementation change
  # # no error
  # skip_if_not_installed("gRain")
  # vt <- voting[1:10, ] 
  # a <- aode('Class', vt)   
  # a <- lp(a, vt, smooth = 1) 
  # p <- predict(a, vt, prob = TRUE)     
  # gRain implementation change
})  