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
 make_cpt_object(tn$.params$buying, class_var = class_var(tn)) 
 # todo: need to somehow test this
}) 

test_that("Data and unmodified", {  
 tn <- lp(nb('class', car[ , 5:7]), car[ , 5:7], smooth = 1)
 mod <- tn
 db <- car
 p <- predict(tn, db)
 expect_equal(db, car)
 expect_equal(db, car)
 expect_equal(tn, mod)
})    

test_that("Bug", {
 skip("Local rdata file")
 load('~/code/bnclassify-client/rdata/tmp-debug.rdata') 
 gr <- candidate_dags[[7]] 
 gr <- lp_implement(gr, .mem_cpts = train[[1]])
 predict(gr, test[[1]])
 compute_joint(gr, test[[1]])
 compute_log_joint_complete(gr, test[[1]])
 compute_anb_log_joint_per_class(gr, test[[1]])
 exp(compute_anb_log_joint_per_class(gr, test[[1]]))
}) 

test_that("cpt var values nominal", {
  test_ind <- function() {
    samp <-  function(n) {
      sample(1:n, size = 1)
    } 
    dim <- c(samp(10), samp(10) , samp(10) )
    index <- c(samp(dim[1]), samp(dim[2]), 1)
    ind <- entry_index(index - 1, dim)
    target <- arrayInd(ind + 1, dim)
    expect_true(all(index == target))
  }
  for (i in 1:1e2 ) {
   test_ind()
  }
})