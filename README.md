# bnclassify

[![Travis-CI Build Status](https://travis-ci.org/bmihaljevic/bnclassify.svg?branch=master)](https://travis-ci.org/bmihaljevic/bnclassify)
[![Coverage Status](https://coveralls.io/repos/bmihaljevic/bnclassify/badge.svg)](https://coveralls.io/r/bmihaljevic/bnclassify)

The bnclassify package implements algorithms for learning discrete Bayesian network classifiers from data and estimating their predictive accuracy.

## Install

```{r}
install.packages('devtools')
devtools::install_github('bmihaljevic/bnclassify')
#devtools::install_github('bmihaljevic/bnclassify', build_vignettes = TRUE)
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