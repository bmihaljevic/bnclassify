context("infer-anb-cpp") 

test_that("Missing features", {
  tn <- nbcar()
  expect_error(compute_joint(tn, car[, 1:2]), 
               "Some features missing from data set.")
})

test_that("Single predictor", {
  tn <- lp(nb('class', car[, c(1,7)]), car, smooth = 0)
  pt <- compute_joint(tn, car[, 1:2])
  expect_identical(dim(pt), c(nrow(car), 4L))
})

test_that("0 rows dataset", {
  tn <- nbcar()
  pt <- compute_joint(tn, car[FALSE, ])
  expect_identical(dim(pt), c(0L, 4L))
})

test_that("No features", {
  nb <- bnc_dag(nb_dag('class', NULL), 'class')
  nb <- lp(nb, car, smooth = 1)
  pt <- compute_joint(nb, car)
  expect_equal(as.vector(pt[1, ]), as.vector(log(params(nb)[['class']])))
  
  pt2 <- compute_joint(nb, car[, FALSE])
  expect_equal(pt, pt2)
})

test_that("Make CPT", { 
 tn <- nbcar()
 make_cpt_object(tn$.params$buying) 
})

test_that("To check", {
  expect_true(FALSE)
#   // TODO: class names for all cpts
# // getClasses() in model   
#     
# // Test e.g., for out of bounds 
# 
# // Does making a new object create new memory in R? Would it then be more efficient to avoid sugar?
# // e.g., match and similar. It creates data in R?
# 
# // make function log() that preserve attributes
# 
# // Consider a single row DF, an empty DF, etc.
  
# // find the list of my types. to which does Model correspond?? 
# I need a function that lists all the types. 
  
#   // after joint, all times go up
# // the anyNA call makes it much slower 
# // expr      min       lq      mean    median       uq      max neval
# // {     f = compute_joint(t, dbor) }  886.097  906.663  950.3323  920.5845  948.043  3049.94  2000
# // {     h = bnclassify:::compute_log_joint(t, dbor) } 1143.406 1180.499 1771.0422 1248.9055 1357.745 91274.94  2000
# // > 
}) 

test_that("C++ 11", {
   # DESCRIPTION makes it build with 11. Update makevars?
  expect_true(FALSE)
})