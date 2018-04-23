context("cpp table")

check_unidim <- function(db, rows, cols) {
  a <- unidim_values(db[, cols, drop = FALSE])
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
 check_unidim(v, TRUE, 18)   
})  