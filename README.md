There are many algorithms for learning Bayesian network classifiers but only a few are available in R. Furthermore, estimating the predictive performance of a classifier usually involves some coding. the bayesclass package provides several algorithms for learning Bayesian network classifiers and simplifies performance assessment. 

Concretely, the bayesclass package implements the following classifiers: 

  * The naive Bayes
  * The selective naive Bayes
  * The tree-augmented naive Bayes
  * The selective tree-augmented naive Bayes

The following search heuristics are used to learn the above-mentioned classifiers:

  * Forward/backward sequential selection
  * Forward sequential selection and joining
  * Fackward sequential elimination and joining

Different scores can be used together with a search algorithm. Currently, the only score implemented is:

  * Estimation of predictive performance (accuracy, Cohen's kappa, RMSE, etc.) using resampling

The caret package is used for the rasampling estimation of predictive performance. 