#' Learn discrete Bayesian network classifiers from data.
#' 
#' State-of-the-art algorithms for learning discrete Bayesian network 
#' classifiers from data, with functions prediction, model evaluation and inspection.
#' 
#' The learn more about the package, start with the vignettes:
#'  \code{browseVignettes(package = "bnclassify")}. The following is a list of available
#'   functionalities: 
#' 
#' Structure learning algorithms:
#'   \itemize{
#'     \item \code{\link{nb}}: Naive Bayes (Minsky, 1961)
#'     \item \code{\link{tan_cl}}: Chow-Liu's algorithm for one-dependence estimators (CL-ODE)  (Friedman et al., 1997)
#' \item \code{\link{fssj}}: Forward sequential selection and joining (FSSJ) (Pazzani, 1996)
#' \item \code{\link{bsej}}: Backward sequential elimination and joining (BSEJ)  (Pazzani, 1996)
#' \item \code{\link{tan_hc}}: Hill-climbing tree augmented naive Bayes (TAN-HC)  (Keogh and Pazzani, 2002)
#' \item \code{\link{tan_hcsp}}: Hill-climbing super-parent tree augmented naive Bayes (TAN-HCSP) (Keogh and Pazzani, 2002)
#' \item \code{\link{aode}}: Averaged one-dependence estimators (AODE) (Webb et al., 2005)
#' }
#' 
#' Parameter learning methods (\code{\link{lp}}):
#' 
#' \itemize{
#' \item Bayesian and maximum likelihood estimation
#' \item Weighting attributes to alleviate naive bayes' independence assumption (WANBIA) (Zaidi et al., 2013)
#' \item Attribute-weighted naive Bayes (AWNB)  (Hall, 2007)
#' \item Model averaged naive Bayes (MANB) (Dash and Cooper, 2002)
#' }
#' 
#' Model evaluating:
#' 
#' \itemize{
#'  \item \code{\link{cv}}: Cross-validated estimate of accuracy 
#'  \item \code{\link[=logLik.bnc_bn]{logLik}}: Log-likelihood
#'  \item \code{\link[=AIC.bnc_bn]{AIC}}: Akaike's information criterion (AIC) 
#'  \item \code{\link[=BIC.bnc_bn]{BIC}}: Bayesian information criterion (BIC) 
#' }
#' 
#' Predicting: 
#' \itemize{
#' \item \code{\link[=predict.bnc_fit]{predict}}: Inference for complete and/or incomplete data (the latter through \code{gRain})}
#' 
#' Inspecting models:
#'  \itemize{ 
#'  \item \code{\link[=plot.bnc_dag]{plot}}: Structure plotting (through \code{Rgraphviz})
#'  \item \code{\link[=print.bnc_base]{print}}: Summary 
#'  \item \code{\link{params}}: Access conditional probability tables 
#'  \item \code{\link{nparams}}: Number of free parameters 
#'  \item and more. See \code{\link{inspect_bnc_dag}} and \code{\link{inspect_bnc_bn}}.
#'  } 
#' 
#' @docType package
#' @name bnclassify
#' @importFrom stats predict as.formula complete.cases setNames logLik AIC BIC nobs optim
#' @importFrom graphics plot
#' @importFrom utils combn 
#' 
#' @references Bielza C and Larranaga P (2014), Discrete Bayesian network 
#'   classifiers: A survey. \emph{ACM Computing Surveys}, \bold{47}(1), Article 
#'   5.
#'   
#'   Dash D and Cooper GF (2002). Exact model averaging with naive Bayesian 
#'   classifiers. \emph{19th International Conference on Machine Learning 
#'   (ICML-2002)}, 91-98.
#'   
#'   Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network classifiers.
#'   \emph{Machine Learning}, \bold{29}, pp. 131--163.
#'   
#'   Zaidi NA, Cerquides J, Carman MJ, and Webb GI (2013) Alleviating naive Bayes 
#'   attribute independence assumption by attribute weighting.
#'   \emph{Journal of Machine Learning Research}, \bold{14} pp. 1947--1988.  
#'   
#'   GI. Webb, JR Boughton, and Z Wang (2005) Not so naive bayes: Aggregating one-dependence 
#'   estimators. \emph{Machine Learning}, \bold{58}(1) pp. 5--24.  
#'   
#'   Hall M (2007). A decision tree-based attribute weighting filter for naive 
#'   Bayes. \emph{Knowledge-Based Systems}, \bold{20}(2), pp. 120-126.
#'   
#'   Koegh E and Pazzani M (2002).Learning the structure of augmented Bayesian 
#'   classifiers. In \emph{International Journal on Artificial Intelligence 
#'   Tools}, \bold{11}(4), pp. 587-601.
#'   
#'   Koller D, Friedman N (2009). Probabilistic Graphical Models: Principles and
#'   Techniques. MIT Press.
#'   
#'   Pazzani M (1996). Constructive induction of Cartesian product attributes. 
#'   In \emph{Proceedings of the Information, Statistics and Induction in 
#'   Science Conference (ISIS-1996)}, pp. 66-77
NULL


#' Bayesian network classifier with structure and parameters.
#' 
#' A Bayesian network classifier with structure and parameters. Returned by 
#' \code{\link{lp}} and \code{\link{bnc}} functions. You can use it to classify
#' data (with \code{\link[=predict.bnc_fit]{predict}}). Can estimate its
#' predictive accuracy with \code{\link{cv}}, plot its structure (with 
#' \code{\link[=plot.bnc_dag]{plot}}), print a summary to console 
#' (\code{\link[=print.bnc_base]{print}}), inspect it with functions documented 
#' in \code{\link{inspect_bnc_bn}} and \code{\link{inspect_bnc_dag}}, and
#' convert it to mlr, grain, and graph objects --see \code{\link{as_mlr}} and 
#' \code{\link{grain_and_graph}}.
#' 
#' @name bnc_bn
#' 
#' @examples 
#' data(car)
#' tan <- bnc('tan_cl', 'class', car, smooth = 1)   
#' tan
#' p <- predict(tan, car)
#' head(p)
#' \dontrun{plot(tan)}
#' nparams(tan)
NULL

#' Bayesian network classifier structure.
#' 
#' A Bayesian network classifier structure, returned by functions such as 
#' \code{\link{nb}} and \code{\link{tan_cl}}. You can plot its structure (with 
#' \code{\link[=plot.bnc_dag]{plot}}), print a summary to console 
#' (\code{\link[=print.bnc_base]{print}}), inspect it with functions documented
#' in \code{\link{inspect_bnc_dag}}, and convert it to a graph object with 
#' \code{\link{grain_and_graph}}.
#' 
#' @name bnc_dag
#' 
#' @examples 
#' data(car)
#' nb <- tan_cl('class', car)   
#' nb
#' \dontrun{plot(nb)}
#' narcs(nb)
NULL

#' Congress Voting Data Set.
#' 
#' Data set from the UCI repository
#' \url{https://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records}.
#' 
#' @source \url{http://goo.gl/GTXrCz}
#' @format A \code{data.frame} with 17 columns and 435 rows.
#' @docType data
#' @name voting
NULL

#' Car Evaluation Data Set.
#' 
#' Data set from the UCI repository:
#' \url{https://archive.ics.uci.edu/ml/datasets/Car+Evaluation}.
#' 
#' @source \url{http://goo.gl/GTXrCz}
#' @format A \code{data.frame} with 7 columns and 1728 rows.
#' @docType data
#' @name car
NULL

#' Learn Bayesian network classifiers in a a greedy wrapper fashion.
#' 
#' Greedy wrapper algorithms for learning Bayesian network classifiers. All 
#' algorithms use cross-validated estimate of predictive accuracy to evaluate 
#' candidate structures.
#' 
#' @name greedy_wrapper
#'   
#' @inheritParams nb
#' @inheritParams cv
#' @inheritParams learn_params
#' @param kdbk An integer. The maximum number of feature parents per feature.
#' @param epsilon A numeric. Minimum absolute improvement in accuracy required 
#'   to keep searching.
#' @param cache_reset A numeric. Number of iterations after which to reset the 
#'   cache of conditional probability tables. A small number reduces the amount
#'   of memory used. \code{NULL} means the cache is never reset (the default).
#' @return A \code{\link{bnc_dag}} object.
#' 
#' @examples 
#' data(car)
#' tanhc <- tan_hc('class', car, k = 5, epsilon = 0)  
#' \dontrun{plot(tanhc)}
#'   
#' @references Pazzani M (1996). Constructive induction of Cartesian product 
#'   attributes. In \emph{Proceedings of the Information, Statistics and 
#'   Induction in Science Conference (ISIS-1996)}, pp. 66-77
#'   
#'   Koegh E and Pazzani M (2002).Learning the structure of augmented Bayesian 
#'   classifiers. In \emph{International Journal on Artificial Intelligence 
#'   Tools}, \bold{11}(4), pp. 587-601.
NULL

#' Learns a one-dependence estimator using Chow-Liu's algorithm.
#' 
#' Learns a one-dependence Bayesian classifier using Chow-Liu's algorithm, by 
#' maximizing either log-likelihood, the AIC or BIC scores; maximizing 
#' log-likelihood corresponds to the well-known tree augmented naive Bayes 
#' (Friedman et al., 1997). When maximizing AIC or BIC the output might be a
#' forest-augmented rather than a tree-augmented naive Bayes.
#' 
#' @name tan_chowliu
#'   
#' @inheritParams nb
#' @param root A character. The feature to be used as root of the augmenting 
#'   tree. Only one feature can be supplied, even in case of an augmenting 
#'   forest. This argument is optional.
#' @param score A character. The score to be maximized. \code{'loglik'}, 
#'   \code{'bic'}, and \code{'aic'} return the maximum likelihood, maximum BIC 
#'   and maximum AIC tree/forest, respectively.
#' @return A \code{\link{bnc_dag}} object.
#'   
#' @references Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network 
#'   classifiers. \emph{Machine Learning}, \bold{29}, pp. 131--163.
#' @examples 
#' data(car)
#' ll <- tan_cl('class', car, score = 'loglik')   
#' \dontrun{plot(ll)}
#' ll <- tan_cl('class', car, score = 'loglik', root = 'maint')   
#' \dontrun{plot(ll)}
#' aic <- tan_cl('class', car, score = 'aic')   
#' bic <- tan_cl('class', car, score = 'bic')   
NULL

#' Learn the parameters of a Bayesian network structure.
#' 
#' Learn parameters with maximum likelihood or Bayesian estimation, the 
#' weighting attributes to alleviate naive bayes' independence assumption (WANBIA), 
#' attribute weighted naive Bayes (AWNB), or the model averaged naive Bayes 
#' (MANB) methods. Returns a \code{\link{bnc_bn}}.
#' 
#' \code{lp} learns the parameters of each local distribution \eqn{\theta_{ijk} 
#' = P(X_i = k \mid \mathbf{Pa}(X_i) = j)}{\theta[ijk] = P(X[i] = k | Pa(X[i]) =
#' j)} as \deqn{\theta_{ijk} = \frac{N_{ijk} + \alpha}{N_{ ij \cdot } + r_i 
#' \alpha},}{\theta[ijk] = (N[ijk] + \alpha) / (N[ ij . ] + r[i] \alpha),} where
#' \eqn{N_{ijk}}{N[ijk]} is the number of instances in \code{dataset} in which 
#' \eqn{X_i = k}{X[i] = k} and \eqn{\mathbf{Pa}(X_i) = j}{Pa(X[i]) = j}, 
#' \eqn{N_{ ij \cdot} = \sum_{k=1}^{r_i} N_{ijk}}{N[ ij . ] = \sum[k=1]^(r[i]) 
#' N[ijk]}, \eqn{r_i}{r[i]} is the cardinality of \eqn{X_i}{X[i]}, and all 
#' hyperparameters of the Dirichlet prior equal to \eqn{\alpha}. \eqn{\alpha = 
#' 0} corresponds to maximum likelihood estimation. Returns a uniform 
#' distribution when \eqn{N_{ i j \cdot } + r_i \alpha = 0}{N[ ij . ] + r[i] 
#' \alpha = 0}. With partially observed data, the above amounts to 
#' \emph{available case analysis}.
#' 
#' WANBIA learns a unique exponent 'weight' per feature. They are 
#' computed by optimizing conditional log-likelihood, and are bounded with
#' all \eqn{w_i \in [0, 1]}. For WANBIA estimates, set \code{wanbia} to \code{TRUE}.
#' 
#' In order to get the AWNB parameter estimate, provide either the 
#' \code{awnb_bootstrap} and/or the \code{awnb_trees} argument. The estimate is:
#' \deqn{\theta_{ijk}^{AWNB} = \frac{\theta_{ijk}^{w_i}}{\sum_{k=1}^{r_i} 
#' \theta_{ijk}^{w_i}},}{\theta[ijk]^(AWNB) = (\theta[ijk])^w[i] / 
#' \sum[k=1]^(r[i]) (\theta[ijk])^(w[i]),} while the weights \eqn{w_i}{w[i]} are
#' computed as \deqn{w_i = \frac{1}{M}\sum_{t=1}^M \sqrt{\frac{1}{d_{ti}}},}{w_i
#' = (1 / M)\sum_[t=1]^M \sqrt{1 / d[ti]},} where \eqn{M} is the number of 
#' bootstrap samples from \code{dataset} and \eqn{d_{ti}}{d[ti]} the minimum 
#' testing depth of \eqn{X_i}{X[i]} in an unpruned classification tree learned 
#' from the \eqn{t}-th subsample (\eqn{d_{ti} = 0}{d[ti] = 0} if \eqn{X_i}{X_i} 
#' is omitted from \eqn{t}-th tree).
#' 
#' The MANB parameters correspond to Bayesian model averaging over the naive 
#' Bayes models obtained from all \eqn{2^n}{2^n} subsets over the \eqn{n} 
#' features. To get MANB parameters, provide the \code{manb_prior} argument. 
#'  
#' @name learn_params
#'   
#' @inheritParams nb
#' @inheritParams inspect_bnc_dag
#' @param dataset The data frame from which to learn network parameters.
#' @param smooth A numeric. The smoothing value (\eqn{\alpha}) for Bayesian 
#'   parameter estimation. Nonnegative.
#' @param awnb_trees An integer. The number (\eqn{M}) of bootstrap samples to 
#'   generate.
#' @param awnb_bootstrap A numeric. The size of the bootstrap subsample, 
#'   relative to the size of \code{dataset} (given in [0,1]).
#' @param manb_prior A numeric. The prior probability for an arc between the 
#'   class and any feature.
#' @param wanbia A logical. If \code{TRUE}, WANBIA feature weighting is
#'   performed.
#' @return A \code{\link{bnc_bn}} object.
#' @references Hall M (2004). A decision tree-based attribute weighting filter 
#'   for naive Bayes. \emph{Knowledge-based Systems}, \bold{20}(2), 120-126.
#'   
#'   Dash D and Cooper GF (2002). Exact model averaging with naive Bayesian 
#'   classifiers. \emph{19th International Conference on Machine Learning 
#'   (ICML-2002)}, 91-98.
#'   
#'   Pigott T D (2001) A review of methods for missing data. \emph{Educational 
#'   research and evaluation}, \bold{7}(4), 353-383.
#' @examples 
#' data(car)
#' nb <- nb('class', car)
#' # Maximum likelihood estimation
#' mle <- lp(nb, car, smooth = 0)
#' # Bayesian estimaion
#' bayes <- lp(nb, car, smooth = 0.5)
#' # MANB
#' manb <- lp(nb, car, smooth = 0.5, manb_prior = 0.5)
#' # AWNB
#' awnb <- lp(nb, car, smooth = 0.5, awnb_trees = 10)
NULL

#' Inspect a Bayesian network classifier structure.
#' 
#' Functions for inspecting a \code{\link{bnc_dag}} object.
#' 
#' @param x The \code{\link{bnc_dag}} object. The Bayesian network classifier
#'   structure.
#'   
#' @name inspect_bnc_dag
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' narcs(nb)
#' is_ode(nb)
NULL

#' Inspect a Bayesian network classifier (with structure and parameters).
#' 
#' Functions for inspecting a \code{\link{bnc_bn}} object. In addition, you can 
#' query this object with the functions documented in 
#' \code{\link{inspect_bnc_dag}}.
#' 
#' @param x The \code{\link{bnc_bn}} object. The Bayesian network classifier.
#'   
#' @name inspect_bnc_bn
#' @examples  
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' nparams(nb)
#' nb <- bnc('nb', 'class', car, smooth = 1, manb_prior = 0.5)
#' manb_arc_posterior(nb)
#' nb <- bnc('nb', 'class', car, smooth = 1, awnb_bootstrap = 0.5)
#' awnb_weights(nb)
NULL

#' Compute (penalized) log-likelihood.
#' 
#' Compute (penalized) log-likelihood and conditional log-likelihood score of a \code{\link{bnc_bn}} object on
#' a data set. Requires a data frame argument in addition to \code{object}.
#' 
#' log-likelihood =  \eqn{log P(\mathcal{D} \mid \theta)}{log P(D | \theta)},
#' 
#' Akaike's information criterion (AIC) = \eqn{log P(\mathcal{D} \mid \theta) - 
#' \frac{1}{2} |\theta|}{log P(D | \theta) - |\theta| / 2},
#' 
#' The Bayesian information criterion (BIC) score: = \eqn{log P(\mathcal{D} \mid
#' \theta) - \frac{\log N}{2} |\theta|}{log P(D | \theta) - N |\theta| / 2},
#' 
#' where \eqn{|\theta|} is the number of free parameters in \code{object}, 
#' \eqn{\mathcal{D}}{D} is the data set and N is the number of instances in 
#' \eqn{\mathcal{D}}{D}.
#' 
#' \code{cLogLik} computes the conditional log-likelihood of the model. 
#' 
#' @name loglik
#'   
#' @inheritParams predict.bnc_fit
#' @param ... A data frame (\eqn{\mathcal{D}}{D}).
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' logLik(nb, car)   
#' AIC(nb, car)
#' BIC(nb, car)
#' cLogLik(nb, car)   
NULL

#' Convert to graph and gRain.
#' 
#' Convert a \code{\link{bnc_dag}} to \code{graphNEL} and
#' \code{\link[gRain]{grain}} objects.
#' 
#' @name grain_and_graph
#' @inheritParams inspect_bnc_bn   
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' # Requires the grain and graph packages installed
#' \dontrun{g <- as_grain(nb)}
#' \dontrun{gRain::querygrain.grain(g)$buying}
NULL

#' @useDynLib bnclassify
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("bnclassify", libpath)
}
