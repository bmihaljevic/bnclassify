context("cpp table")

check_unidim <- function(db, rows, cols) {
  a <- table_cpp(db[, cols, drop = FALSE])
  b <- table(db[, cols, drop = FALSE])
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