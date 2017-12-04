context("learn params wanbia")

test_that('make cll', {
  make_cll('Class', v)(rep(1, 16)) 
  # Too few weights
  expect_error(make_cll('Class', v)(rep(1, 15)))
})

get_accus <- function(class_var, w, dataset) {
  nb <- lp(nb(class_var, dataset), dataset, smooth = 0)
  wanb <- set_weights(nb, w)
  p <- predict(nb, dataset)
  pw <- predict(wanb, dataset) 
  c( accuracy(p, dataset[[class_var]]),  accuracy(pw, dataset[[class_var]]) )
}

test_that('datasets', {
  w <- compute_wanbia_weights('Class', v)  
  acc <- get_accus('Class', w, v)   
  expect_true(acc[2] > acc[1]) 
  
  kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff')
  w <- compute_wanbia_weights('class', kr)
  acc <- get_accus('class', w, kr)
  expect_true(acc[2] > acc[1])
})  

# summary wanbia experiments
# - smooth? no, they used 'm-estimates' 
#   - I can check this is their code
# - incomplete data?
# - folds? stratified?