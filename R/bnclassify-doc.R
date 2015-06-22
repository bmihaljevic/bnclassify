#' Algorithms for learning Bayesian network classifiers from data.
#' 
#' The bnclassify package implements algorithms for learning discrete Bayesian 
#' networks from data. Learning can be performed on both complete and incomplete
#' data whereas prediction is much slower in the latter case and thus wrapper 
#' learners are not recommended in that case.
#' 
#' Structure learning algorithms \itemize{ \item Naive Bayes \code{\link{nb}}
#' \item Tree augmented naive Bayes \code{\link{tan_cl}} \item Backward
#' sequential elimination and joining \code{\link{bsej}} \item Forward
#' sequential selection and joining \code{\link{fssj}} \item Hill-climbing tree 
#' augmented naive Bayes \code{\link{tan_hc}} \item Hill-climbing super-parent
#' tree augmented naive Bayes \code{\link{tan_hcsp}}  }
#' 
#' Parameter estimation \itemize{ \item Attribute-weighted naive Bayes
#' \code{\link{lp_awnb}} }
#' 
#' 
#' @docType package
#' @name bnclassify
#' @importFrom stats predict
#' @references Bielza C and Larranaga P (2014), Discrete Bayesian network 
#'   classifiers: A survey. \emph{ACM Computing Surveys}, \bold{47}(1), Article 
#'   5.
#'   
#'   Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network classifiers.
#'   \emph{Machine Learning}, \bold{29}, pp. 131--163.
#'   
#'   Hall M (2007). A decision tree-based attribute weighting filter for naive 
#'   Bayes. \emph{Knowledge-Based Systems}, \bold{20}(2), pp. 120-126.
#'   
#'   Pazzani M (1996). Constructive induction of Cartesian product attributes. 
#'   In \emph{Proceedings of the Information, Statistics and Induction in 
#'   Science Conference (ISIS-1996)}, pp. 66-77
#'   
#'   Koegh E and Pazzani M (2002).Learning the structure of augmented Bayesian 
#'   classifiers. In \emph{International Journal on Artificial Intelligence 
#'   Tools}, \bold{11}(4), pp. 587-601.
#'   
#'   Blanco R and Inza I and Merino M and Quiroga J and Larranaga P (2005), 
#'   Feature selection in Bayesian classifiers for the prognosis of survival of 
#'   cirrhotic patients treated with TIPS. \emph{Journal of Biomedical 
#'   Informatics}, \bold{38}(5), pp. 376--388.
NULL

#' Congress Voting Data Set.
#' 
#' @source \url{http://sourceforge.net/projects/weka/files/datasets/UCI and StatLib/uci-20070111.tar.gz}
#' @format A \code{data.frame} with 17 columns and 435 rows.
#' @docType data
#' @name voting
NULL

#' Car Evaluation Data Set.
#' 
#' @source \url{http://sourceforge.net/projects/weka/files/datasets/UCI and StatLib/uci-20070111.tar.gz}
#' @format A \code{data.frame} with 7 columns and 1728 rows.
#' @docType data
#' @name car
NULL

#' Learns Bayesian network classifiers in a wrapper fashion.
#' 
#' bsej is the backward \emph{sequential elimination and joining} algorithm
#' whereas fssj is the \emph{forward sequential selection and joining}
#' algorithms for learning a semi-naive Bayes classifier (Pazzani, 1996). tan_hc
#' Learns a tree augmented naive Bayes with a greedy hill-climbing search. tanhc
#' is the super-parent variant of tan_hc.
#' 
#' @name wrapper
#'   
#' @inheritParams nb 
#' @param epsilon A numeric. Minimum absolute improvement required to keep 
#'   searching.
#' @inheritParams learn_params 
#'   
#' @references Pazzani M (1996). Constructive induction of Cartesian product 
#'   attributes. In \emph{Proceedings of the Information, Statistics and 
#'   Induction in Science Conference (ISIS-1996)}, pp. 66-77
#'   
#'   Koegh E and Pazzani M (2002).Learning the structure of augmented Bayesian 
#'   classifiers. In \emph{International Journal on Artificial Intelligence 
#'   Tools}, \bold{11}(4), pp. 587-601.
NULL

#' Learns a tree augmented naive Bayes classifier (TAN).
#' 
#' Learns a one-dependence Bayesian classifier using Chow-Liu's algorithm. The 
#' structure is learned so that either likelihood, the BIC or AIC scores are 
#' maximized; the first option corresponds to the well-known tree augmented 
#' naive Bayes (Friedman et al., 1997).
#' 
#' Edges with negative BIC or AIC weights are blacklisted from the final 
#' structure. The structure thus might be a forest rather than a tree.
#' 
#' @name tan_chowliu
#'   
#' @inheritParams nb 
#' @param blacklist A character matrix. Edges that may be blacklisted from the
#'   resulting structure.
#' @param root A character. The feature to be used as root of the augmenting 
#'   tree. Only one feature can be supplied, even in case of an augmenting 
#'   forest. This argument is optional.
#' @param score A character. The score to be maximized. \code{'loglik'},
#'   \code{'bic'}, and \code{'aic'} return the maximum likelihood, maximum BIC
#'   and maximum AIC tree/forest, respectively.
#'   
#' @references Friedman N, Geiger D and Goldszmidt M (1997). Bayesian network 
#'   classifiers. \emph{Machine Learning}, \bold{29}, pp. 131--163.
NULL

#' Learn the parameters of a Bayesian network structure.
#' 
#' \code{lpawnb} weights the features' CPTs according to the AWNB method.
#' 
#' @name learn_params
#'   
#' @param x a \code{\link{bnc_dag}} object. The Bayesian network structure.
#' @param dataset The data frame from which to estimate network parameters.
#' @param smooth A nonnegative numeric. The smoothing value for Bayesian 
#'   parameter estimation.
#' @param trees An integer. The number of bootstrap samples to generate.
#' @param bootstrap_size A numeric. The size of the bootstrap subsample,
#'   relative to the size of \code{dataset} (given in [0,1]).
#' @references Mark Hall (2004). A decision tree-based attribute weighting 
#'   filter for naive Bayes. \emph{Knowledge-based Systems}, \bold{20}(2), 
#'   120-126.
NULL