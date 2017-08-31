test_that('make cll', {
  make_cll('Class', v)(rep(1, 16))
})

get_accus <- function(class_var, w, dataset) {
  nb <- lp(nb(class_var, dataset), dataset, smooth = 1)
  wanb <- set_weights(nb, w)
  p <- predict(nb, dataset)
  pw <- predict(wanb, dataset) 
  c( accuracy(p, dataset[[class_var]]),  accuracy(pw, dataset[[class_var]]) )
}

test_that('datasets', {
  w <- compute_wanbia_weights('Class', v)  
  names(w)[w > 0.3] 
  get_accus('Class', w, v) 
  
  
  w <- compute_wanbia_weights('class', car)  
  names(w)[w > 0.3]  
  get_accus('class', w, car) 
  
  # kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff') 
  # w <- compute_wanbia_weights('class', kr)   
  get_accus('class', w, kr) 
})
