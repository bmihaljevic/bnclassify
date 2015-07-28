bnclassify
==========

[![Travis-CI Build Status](https://travis-ci.org/bmihaljevic/bnclassify.svg?branch=master)](https://travis-ci.org/bmihaljevic/bnclassify) [![codecov.io](https://codecov.io/github/bmihaljevic/bnclassify/coverage.svg?branch=master)](https://codecov.io/github/bmihaljevic/bnclassify?branch=master)

The bnclassify package implements algorithms for learning discrete Bayesian network classifiers from data and estimating their predictive accuracy.

Example
=======

Let us load a data set and learn a tree-augmented naive Bayes by maximizing the BIC score.

``` r
library(bnclassify)
data(car)
tn <- tan_cl('class', car, score = 'bic')
tn
#> 
#>   Bayesian network classifier
#> 
#>   class variable:        class 
#>   num. features:   6 
#>   arcs:   6 
#>   learning algorithm:    tan_cl
plot(tn)
```

![](README-unnamed-chunk-2-1.png)

After we fit its parameters, we can use it to predict class labels.

``` r
tn <- lp(tn, car, smooth = 0.01)
p <- predict(tn, car, prob = TRUE)
head(p)
#>          unacc          acc         good        vgood
#> [1,] 1.0000000 2.171346e-10 8.267214e-16 3.536615e-19
#> [2,] 0.9999937 6.306269e-06 5.203338e-12 5.706038e-19
#> [3,] 0.9999908 9.211090e-06 5.158884e-12 4.780777e-15
#> [4,] 1.0000000 3.204714e-10 1.084552e-15 1.015375e-15
#> [5,] 0.9999907 9.307467e-06 6.826088e-12 1.638219e-15
#> [6,] 0.9999864 1.359469e-05 6.767760e-12 1.372573e-11
p <- predict(tn, car, prob = FALSE)
head(p)
#> [1] unacc unacc unacc unacc unacc unacc
#> Levels: unacc acc good vgood
```

Estimate predictive accuracy with cross validation.

``` r
cv(tn, car, k = 10, smooth = 0.01, dag = TRUE)
#> [1] 0.8657653
```

Install
=======

Make sure you have at least version 3.2.0 of R. You will need to install packages from Bioconductor.

``` r
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph", "RBGL", "Rgraphviz"))
```

When running the above yoy may get asked whether you would like to update some packages. My approach is to say no in order to avoid any possible conflict with packages on CRAN (you can update those packages from CRAN if needed).

When you read this bnclassify may already be on CRAN. In that case, use:

``` r
install.packages('bnclassify')
```

If not, you can install the current version from github:

``` r
install.packages('devtools')
devtools::install_github('bmihaljevic/bnclassify', build_vignettes = TRUE)
#devtools::install_github('bmihaljevic/bnclassify')
```

Overview
========

See the list of implemented algorithms with

``` r
?bnclassify
```

Use the vignette to get started.

``` r
vignette('bnclassify')
```
