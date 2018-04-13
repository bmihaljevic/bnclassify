kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff')
library(bnclassify)
source('tests/infer-tests-utils.R')

# devtools::load_all(".")
dbor <- kr
t <- lp(nb('class', dbor), dbor, smooth = 1) 

tinfer_consistent(t, dbor) 
tinfer_benchmark(t, dbor)