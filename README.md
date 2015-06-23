# bnclassify

[![Travis-CI Build Status](https://travis-ci.org/bmihaljevic/bnclassify.svg?branch=master)](https://travis-ci.org/bmihaljevic/bnclassify)
[![codecov.io](https://codecov.io/github/bmihaljevic/bnclassify/coverage.svg?branch=master)](https://codecov.io/github/bmihaljevic/bnclassify?branch=master)

The bnclassify package implements algorithms for learning discrete Bayesian network classifiers from data and estimating their predictive accuracy.

## Install

Make sure you have at least version 3.1.0 of R. 

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph", "RBGL", "Rgraphviz"))
install.packages('devtools')
devtools::install_github('bmihaljevic/bnclassify', build_vignettes = TRUE)
#devtools::install_github('bmihaljevic/bnclassify')
```

## Use

See the list of implemented algorithms with 

```{r}
?bnclassify
```

Use the vignette to get started. 
```{r}
vignette('bnclassify')
```