context("cpp table")

check_unidim <- function(db, rows, cols) {
  db <- db[, cols, drop = FALSE]
  a <- table_cpp(db)
  b <- table(db)
  if (ncol(db) == 1) {
   # table sets name of object
   names(dimnames(a)) = "db" 
  }
  expect_equal(a, b)   
}

test_that("1D", {  
 check_unidim(car, TRUE, 1)  
 check_unidim(car, 1:5, 1)    
 check_unidim(car, FALSE, 1)      
 
 check_unidim(v, TRUE, 1)  
 check_unidim(v, 1:5, 1)    
 check_unidim(v, FALSE, 1)      
})

test_that("3D", { 
 check_unidim(car, TRUE, 1:3)  
 check_unidim(car, 1:5, 1:3)    
 check_unidim(car, FALSE, 1:3)      
 
 check_unidim(v, TRUE, 1:3)  
 check_unidim(v, 1:5, 1:3)    
 check_unidim(v, FALSE, 1:3)      
})


test_that("large dim", { 
 check_unidim(car, TRUE, 1:7)  
 check_unidim(v, TRUE, 17)  
})   

test_that("NA", {
  db <- car[, 1:3]
  db[1, 1] <- NA 
  check_unidim(db, TRUE, TRUE)   
  
  db[1:100, 1] <- NA 
  check_unidim(db, TRUE, TRUE)   
})


test_that("random", {
  (x = sample(0:1, 1e5, replace = T))
  x <- data.frame(u = factor(x))
  a <- table_cpp(x)
  b <- table(x)
  # don't know why table keeps x as name.
  expect_equal(unname(a), unname(b))   
})


test_that("ordinal", {
  skip_if_not_installed("mlbench")
  library(mlbench)
  data(Soybean)
  Soybean <- na.omit(Soybean)  
  check_unidim(Soybean, TRUE, c('precip', 'Class'))       
  check_unidim(Soybean, TRUE, c('precip'))       
  load('~/code/bnclassify-client/tmp-input.rdata')
  for (i in 1:1e5) {
    table_cpp(input) 
  }
  load('tmp-input-fails.rdata') 
  for (i in 1:1e5) {
    table_cpp(input) 
  } 
  # fssj            = { set.seed(0); fssj   <- bnc('fssj', 'Class', Soybean, smooth = 1, dag_args = list(k = 5,  epsilon = 0))       } 
})